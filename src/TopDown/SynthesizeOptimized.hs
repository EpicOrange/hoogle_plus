{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.SynthesizeOptimized(synthesize, envToGoal, synO, synGuardO, synO', synGuardO') where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import TopDown.Size
import TopDown.GoalTrace
import HooglePlus.GHCChecker (check)
import HooglePlus.Synthesize (envToGoal)
import Database.Convert (addTrue)
import Synquid.Error
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Logic
import Synquid.Type
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Program
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import PetriNet.Util

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)

-- a wrapper for synthesize that you can run from `stack ghci`
-- usage: syn "Int -> Int"
-- or:    program <- syn "Int -> Int"
synO :: String -> IO ()
synO inStr = synO' inStr []
-- What if we can pass the guardInclude list into here??? :D
  
synGuardO :: String -> [String] -> IO ()
synGuardO inStr guards = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  let rawSyms = Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols
  goal <- envToGoal (env { _symbols = rawSyms}) inStr
  solverChan <- newChan
  synthesize defaultSearchParams goal [] solverChan


-- usage:
-- :{
-- syn' "(a -> b) -> (a -> c) -> a -> (b, c)" $ [
--   [ (["\\x -> x + 1", "\\x -> x * 3", "3"], "(4, 9)")
--   , (["\\x -> x ++ x", "Data.List.reverse", "[1,2,3]"], "([1,2,3,1,2,3], [3,2,1])")
--   ]
-- :}
synO' :: String -> [([String], String)] -> IO ()
synO' inStr ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  goal <- envToGoal env inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize defaultSearchParams goal examples solverChan

synGuardO' :: String -> [String] -> [([String], String)] -> IO ()
synGuardO' inStr guards ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  let rawSyms = Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols
  goal <- envToGoal (env { _symbols = rawSyms}) inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize defaultSearchParams goal examples solverChan

synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
synthesize searchParams goal examples messageChan = do
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal :: RSchema
    -- let destinationType = lastType (toMonotype goalType)

    -- TODO use these flags!!!!!!!!!!
    let useHO = _useHO searchParams
    let useAltIMode = _topDownUseAltIMode searchParams
    let useMemoize = _topDownUseMemoize searchParams
    let printBacktrace = _topDownPrintBacktrace searchParams

    let rawSyms = rawEnv ^. symbols

    let symbolsWithoutFUN = Map.filterWithKey (\k a -> not $ "'ho'" `isInfixOf` k) rawSyms
    let env = rawEnv { _symbols = symbolsWithoutFUN, _hoCandidates = [] }

    -- putStrLn "\n=================="
    -- putStrLn "Starting!"
    -- printf "Arguments: %s\n" (show $ env ^. arguments)
    -- let goal = shape $ lastType $ toMonotype goalType :: SType

    -- printf "Goal: %s\n" (show goal)
    -- mapM_ print (Map.keys $ envWithHo ^. symbols)
    -- putStrLn "=================="

    -- call dfs with iterativeDeepening
    program <- iterativeDeepening env messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    -- return program
    return ()


data SynMode = IMode | EMode deriving (Eq, Ord, Show)
type MemoMap = Map (SynMode, RType, Int, Map Id SType) (Logic (RProgram, Map Id SType)) -- (mode, query, quota, sub) ==> program
type TopDownSolver m = StateT CheckerState (StateT GoalTrace (LogicT (StateT MemoMap m)))
-- data OurStuff = OurStuff { backtrace :: GoalTrace }
-- emptyOurStuff = OurStuff []

evalTopDownSolver :: Monad m => RType -> Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolver goalType messageChan m =
  (`evalStateT` Map.empty) $ observeT $ msum $ map (g . f) m
  where
    f = (`evalStateT` emptyChecker {_checkerChan = messageChan})
    g = (`evalStateT` [mkHole goalType])

-- convert to Logic a
choices :: (Traversable f, MonadPlus m) => f a -> m a
choices = msum . fmap return

memoizeProgram :: SynMode -> Int -> RType -> TopDownSolver IO RProgram -> TopDownSolver IO RProgram
-- memoizeProgram _ _ compute = compute
memoizeProgram mode quota goalType compute = do
  st <- get
  let sub = st ^. typeAssignment
  let key = (mode, goalType, quota, sub)
  memoMap <- lift $ lift $ get :: TopDownSolver IO MemoMap
  -- TODO maybe use Map.lookup with default compute??????
  -- TODO should use the de brujin index once this all works
  case Map.lookup key memoMap of
    -- retrieve stored value
    Just progs -> do
      -- liftIO $ printf "\nomg we're actually using memoize!!! \n\tquery: %s\t\t%s \n\t\tsub: %s\n" (show goalType) (show key) (show sub)
      (prog, sub') <- choices progs 
      -- liftIO $ printf "\t prog: %s :: %s\n" (show prog) (show $ typeOf prog)
      -- liftIO $ printf "\t \tsub': %s\n" (show sub')

      -- omg we're actually using memoize!!! 
      
      let sub'' = Map.map (stypeSubstitute sub') sub <> sub'
      modify $ set typeAssignment sub''

      -- when ("fst" `isInfixOf` show prog || "snd" `isInfixOf` show prog) $ liftIO $ printf "quota (%d): we found one! goal (%s), prog (%s)\n" quota (show goalType) (show prog)
      -- liftIO $ printf "key and program retrieved:\n\t%s\n\t%s\n" (show key) (show prog)
      return prog
    -- compute and store value
    Nothing    -> do
      -- TODO we're only storing nonempty streams, should we store mzero too???
      -- e.g if you synthesize goal T with quota 10, and it turns out nothing is size 10 of type T,
      --     you don't want to redo all that computation just to come up with nothing
      --     for the next time you synthesize goal T with quota 10
      -- TODO need to only add to memo map after we're completely done with compute
      -- so, do something like
      -- prog <- compute `mplus` (...store things... >> return mzero)
      prog <- compute
      
      -- liftIO $ printf "key and program added:\n\t%s\n\t%s\n" (show key) (show prog)

      st' <- get
      let sub' = st' ^. typeAssignment
      lift $ lift $ modify (Map.insertWith mplus key (return (prog, sub')))
      return prog

-- type ProgramTrace = (RProgram, [OurProgram])

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver goalType messageChan $ (`map` [1..50]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota

  -- plotted our tests, and solutions tend to have sub size = 3.7 * program size
  -- tests say (sub quota = 3 * program size quota) is best
  solution <- dfs EMode env messageChan quota (quota * 3) goalType :: TopDownSolver IO RProgram
  lift $ modify (\goalTrace -> Symbol (show solution) : goalTrace) -- append solution to trace

  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution env)
  
  -- call check on the program
  guard =<< liftIO (check' solution)

  subSize <- sizeOfSub
  liftIO $ printf "\n\n(Quota %d) Done with %s!\nsize\tsubSize\tsolution\n%d\t%d\t%s\n\n" quota (show goal) (sizeOfProg solution) subSize (show solution)
  lift printGoalTrace

  return solution
  where
    goalType :: RType
    goalType = lastType $ toMonotype goal 

    -- wrapper for `check` function
    check' :: RProgram -> IO Bool
    check' program = do
      checkResult <- check env searchParams examples program goal messageChan `evalStateT` emptyFilterState
      case checkResult of
        Nothing  -> return False
        Just exs -> do
          out <- toOutput env program exs
          printResult $ encodeWithPrefix out
          return True

-- determines if the result has all the appropriate arguments ()
filterParams :: RProgram -> Environment -> Bool
filterParams program env = all (`isInfixOf` show program) $ filter (not . ("tcarg" `isInfixOf`) ) $ Map.keys $ env ^. arguments

--
-- does DFS in either E-mode or I-mode
-- in E-mode:
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
-- in I-mode:
--    * checks if anything in the environment matches the full type of goal
--    * if is a function type, add args to env, search for the return type, and return a lambda 
--    * if not, search in e-mode
--
dfs :: SynMode -> Environment -> Chan Message -> Int -> Int -> RType -> TopDownSolver IO RProgram
dfs mode env messageChan sizeQuota subQuota goalType 
  | sizeQuota <= 0 = mzero
  | otherwise      = do
      (holedProgram:_) <- lift get
      -- guard (subSize > subQuota)
      liftIO $ printf "current goal: %s\n" (show holedProgram)
      -- lift $ modify (\goalTrace -> holedProgram : goalTrace)
      
      prog <- inEnv `mplus` doSplit mode

      guardCheck prog
      
      return prog

      -- st <- get
      -- let sub = st ^. typeAssignment
      -- subSize <- sizeOfSub
      -- liftIO $ printf "<< sizeQuota %d, subQuota %d >>: sizeOfProg (%s) = %d, %d\n" sizeQuota subQuota (show prog) (sizeOfProg prog) (subSize)
      -- liftIO $ mapM_ (printf "\t* %s\n" . show) $ Map.toList sub
      -- liftIO $ printf "<< sizeQuota %d, subSize %d >>: sizeOfProg (%s :: %s) = %d\n" sizeQuota  (subSize) (show prog) (show $ typeOf prog) (sizeOfProg prog)
  where
    -- return components whose entire type unify with goal type
    inEnv = do 
      (id, schema) <- getUnifiedComponent :: TopDownSolver IO (Id, SType)
      return Program { content = PSymbol id, typeOf = addTrue schema }

    -- add args to env, and search for the return type
    doSplit IMode = case goalType of
      ScalarT _ _            -> doSplit EMode
      FunctionT _ tArg tBody -> do
        argName <- freshId (Map.keys $ env ^. arguments) "arg"

        -- add argument to new env and call dfs IMode with that new env
        let env' = addVariable argName tArg $ addArgument argName tArg env

        -- update buckets
        -- we're synthesizing the body for the lambda
        -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
        lift $ addLam argName (show tArg)
        body <- dfs IMode env' messageChan (sizeQuota - 1) subQuota tBody

        let program = Program { content = PFun argName body, typeOf = goalType }
        guard (sizeOfProg program <= sizeQuota)

        return program

    -- split goal into 2 goals: alpha -> T and alpha
    doSplit EMode = do      
      let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
      let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema
      
      -- need to save the name counter so it generates the same tau for each freshType call
      nameCtr <- getNameCounter
      alpha <- freshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') :: TopDownSolver IO RType
      setNameCounter nameCtr
      schema <- freshType (env ^. boundTypeVars) schema' :: TopDownSolver IO RType


      -- we split (?? :: T) into (??::alpha -> T) (??::alpha)
      -- subtract 1 from quota since the second term should be at least size 1
      -- need to save the last trace program so it replaces the right hole after the first dfs
      (holedProgram:_) <- lift get
      lift $ addApp (show schema) (show alpha)
      alphaTProgram <- dfs EMode env messageChan (sizeQuota - 1) subQuota schema :: TopDownSolver IO RProgram

      sub <- use typeAssignment
      let alphaSub = addTrue $ stypeSubstitute sub (shape alpha) :: RType 
      
      lift $ addAppFilled alphaTProgram (show alpha) holedProgram
      alphaProgram <- dfs IMode env messageChan (sizeQuota - sizeOfProg alphaTProgram) subQuota alphaSub :: TopDownSolver IO RProgram
      
      return Program {
          content = case content alphaTProgram of
                      PSymbol id -> PApp id [alphaProgram]
                      PApp id xs -> PApp id $ xs ++ [alphaProgram],
          typeOf = goalType
        }

    -- guards away programs that we don't want
    guardCheck :: RProgram -> TopDownSolver IO ()
    guardCheck prog = do
      guard $ not $ "Data.Maybe.fromJust Data.Maybe.Nothing" `isInfixOf` show prog
      guard $ not $ "GHC.List.head []" `isInfixOf` show prog
      guard $ not $ "GHC.List.last []" `isInfixOf` show prog
      guard (sizeOfProg prog <= sizeQuota)
      subSize <- sizeOfSub
      guard (subSize <= subQuota)


    -- Using the components in env, like ("length", <a>. [a] -> Int)
    -- tries to instantiate each, replacing type vars in order to unify with goalType
    getUnifiedComponent :: TopDownSolver IO (Id, SType)
    getUnifiedComponent = do
      (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)


      -- replaces "a" "b" with "tau1" "tau2"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType

      modify $ set isChecked True
      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      st' <- get

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked

      let subbedType = stypeSubstitute sub (shape freshVars)
      -- liftIO $ printf "quota %d %d, (id, schema): %s :: %s\n\tt1: %s\n\tt2: %s\n\tinto: %s\n\tchecks: %s\n\n"
      --   sizeQuota subQuota id (show schema) (show t1) (show t2) (show $ subbedType) (show checkResult)
      
      guard checkResult

      return (id, subbedType)

      where
        -- moves all Data.Function functions to the end and moves the args to the front
        reorganizeSymbols :: [(Id, RSchema)]
        reorganizeSymbols = args ++ withoutDataFunctions

        ogSymbols            = Map.toList $ env ^. symbols
        (args, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) ogSymbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs
