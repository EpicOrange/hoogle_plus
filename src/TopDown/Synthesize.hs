{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, synGuard, syn', synGuard') where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import TopDown.Size
import TopDown.GoalTrace
import HooglePlus.GHCChecker (check)
import HooglePlus.Synthesize (envToGoal)
import Database.Convert (addTrue)
import Synquid.Program
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
import HooglePlus.IOFormat
import PetriNet.Util

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Printf (printf)

synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
synthesize searchParams goal examples messageChan = do
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal :: RSchema

    let useHO = _useHO searchParams -- done

    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates

     -- add higher order functions to the environment
    let env = if useHO
        then do
            let symbolsWithoutFUN = Map.filterWithKey (\k a -> not $ "'ho'" `isInfixOf` k) rawSyms
            rawEnv { 
                _symbols = symbolsWithoutFUN, 
                _hoCandidates = [] 
              }
        else do
            let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
            rawEnv {
                _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
                _hoCandidates = []
              }
    let enableDebug = _topDownEnableDebug searchParams

    when enableDebug $ putStrLn "\n=================="
    when enableDebug $ putStrLn "Starting!"
    when enableDebug $ printf "Arguments: %s\n" (show $ env ^. arguments)
    let goal = shape $ lastType $ toMonotype goalType :: SType

    when enableDebug $ printf "Goal: %s\n" (show goal)
    -- when enableDebug $ mapM_ print (Map.keys $ env ^. symbols)
    when enableDebug $ putStrLn "=================="

    -- call dfsExactly with iterativeDeepening
    program <- iterativeDeepening env messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)








-- 
-- try to get solutions by calling dfsExactly on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
-- iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver goalType messageChan $ (`map` [2,4..]) $ \quota -> do
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver goalType messageChan $ (`map` [1..]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfsExactly on %s at size %d\n" (show goal) quota

  solution <- dfsExactly EMode env Map.empty messageChan searchParams goalType quota :: TopDownSolver IO RProgram
  lift $ modify (\goalTrace -> Symbol (show solution) : goalTrace) -- append solution to trace

  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution)
  
  -- liftIO $ printf "checking program:       %s\n" (show solution)
  -- call check on the program
  guard =<< liftIO (check' solution)

  subSize <- sizeOfSub
  debug $ printf "\n\n"
  
  when enableDebug $ lift printGoalTrace
  
  let progSize = sizeOfContent solution
  debug $ printf "\n(Quota %d) Done with %s!\nsize +\tsubSize\tsolution\n%d\t%d\t%s\n\n" quota (show goal) progSize subSize (show solution)
 
  when enableDebug $ printSub

  return solution
  where
    goalType :: RType
    goalType = lastType $ toMonotype goal 

    -- determines if the result has all the appropriate arguments ()
    filterParams :: RProgram -> Bool
    filterParams program = all (`elem` programIds) $ Map.keys $ env ^. arguments 
      where
        programIds :: [Id]
        programIds = flattenProgramIds $ content program
        
        flattenProgramIds :: BareProgram RType -> [Id]
        flattenProgramIds (PSymbol id) = [id]
        flattenProgramIds (PApp id progs) = id : concatMap (flattenProgramIds . content) progs
        flattenProgramIds (PFun id prog) = id : (flattenProgramIds . content) prog

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

    enableDebug = _topDownEnableDebug searchParams

    -- only prints things when we've enabled debugging
    debug :: (MonadIO m) => IO () -> m ()
    debug m = when enableDebug $ liftIO m

  







dfsUpTo :: SynMode -> Environment -> Map Id RType -> Chan Message -> SearchParams -> RType -> Int -> TopDownSolver IO RProgram
dfsUpTo mode env args messageChan searchParams goalType sizeQuota = do
  msum $ map (dfsExactly mode env args messageChan searchParams goalType) [1..sizeQuota]








--
-- does DFSExactly in either E-mode or I-mode
-- in E-mode:
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
-- in I-mode:
--    * checks if anything in the environment matches the full type of goal
--    * if is a function type, add args to env, search for the return type, and return a lambda 
--    * if not, search in e-mode
--
dfsExactly :: SynMode -> Environment -> Map Id RType -> Chan Message -> SearchParams -> RType -> Int -> TopDownSolver IO RProgram
dfsExactly mode env args messageChan searchParams goalType sizeQuota
  | sizeQuota <= 0 = mzero
  | useMemoize     = error "broken, don't use memoize" -- memoizeProgram mode sizeQuota args goalType doDfsExactly
  | otherwise      = doDfsExactly
  where

    -- | does DFSExactly without memoization
    doDfsExactly :: TopDownSolver IO RProgram
    doDfsExactly = do
      memoMap <- lift $ lift $ get :: TopDownSolver IO MemoMap
      
      prog <- inEnv `mplus` doSplit mode
      
      guardCheck prog

      return prog

    -- search params
    useAltIMode = _topDownUseAltIMode searchParams
    useMemoize  = _topDownUseMemoize searchParams
    enableDebug = _topDownEnableDebug searchParams

    -- | only prints things when we've enabled debugging
    debug :: (MonadIO m) => IO () -> m ()
    debug m = when enableDebug $ liftIO m
    
    -- | return components whose entire type unify with goal type
    inEnv :: TopDownSolver IO RProgram
    inEnv = do
      -- only check env if e mode, or if we're using the alt I mode
      guard (useAltIMode || mode == EMode)
      (id, schema) <- getUnifiedComponent :: TopDownSolver IO (Id, SType)
      return Program { content = PSymbol id, typeOf = addTrue schema }

    -- | add args to env, and search for the return type
    doSplit :: SynMode -> TopDownSolver IO RProgram
    doSplit IMode = case goalType of
      ScalarT _ _            -> doSplit EMode
      FunctionT _ tArg tBody -> do
        argName <- freshId (Map.keys $ env ^. arguments) "arg"

        -- add argument to new env and call dfsExactly IMode with that new env
        let env' = addVariable argName tArg $ addArgument argName tArg env
        let args' = Map.insert argName tArg args

        -- we're synthesizing the body for the lambda
        -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
        lift $ addLam argName (show tBody)
        body <- dfsExactly IMode env' args' messageChan searchParams tBody (sizeQuota - 1)

        let program = Program { content = PFun argName body, typeOf = goalType }
        progSize <- sizeOfProg program
        guard (progSize == sizeQuota)

        return program

    -- we split (?? :: T) into (??::alpha -> T) (??::alpha)
    doSplit EMode = do
      let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
      let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema
      
      -- need to save the name counter so it generates the same tau for each ourFreshType call
      nameCtr <- getNameCounter
      alpha <- ourFreshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') "alpha" :: TopDownSolver IO RType
      setNameCounter nameCtr
      schema <- ourFreshType (env ^. boundTypeVars) schema' "alpha" :: TopDownSolver IO RType

      -- need to save the last trace program so it replaces the right hole after the first dfsExactly
      holedProgram <- head <$> lift get
      lift $ addApp (show schema) (show alpha)


      alphaTProgram <- dfsUpTo EMode env args messageChan searchParams schema (sizeQuota - 1) :: TopDownSolver IO RProgram

      sub <- use typeAssignment
      let alphaSub = addTrue $ stypeSubstitute sub (shape alpha) :: RType
      
      lift $ addAppFilled alphaTProgram (show alpha) holedProgram

      alphaProgram <- dfsExactly IMode env args messageChan searchParams alphaSub (sizeQuota - sizeOfContent alphaTProgram) :: TopDownSolver IO RProgram

      return Program {
          content = case content alphaTProgram of
                      PSymbol id -> PApp id [alphaProgram]
                      PApp id xs -> PApp id $ xs ++ [alphaProgram],
          typeOf = goalType
        }

    -- | guards away programs that we don't want
    guardCheck :: RProgram -> TopDownSolver IO ()
    guardCheck prog = do
      -- remove erroring or redundant partial functions
      -- TODO later we'll do this in memoize
      let showProg = show prog
      let uselessSubPrograms = -- these all error or are redundant
                               [ "Data.Maybe.fromJust Data.Maybe.Nothing"
                               , "GHC.List.head []"
                              --  , "GHC.List.head arg0" -- TODO remove this
                               , "GHC.List.last []"
                               , "GHC.List.tail []"
                               , "GHC.List.init []"
                               , "GHC.List.cycle []"
                               , "[] !!"
                               -- these are all []
                               , "[] ++ []"
                               , "GHC.List.zip [] []"
                               , "(GHC.List.!!) []"
                               , "GHC.List.concat []"
                               , "GHC.List.reverse []"
                               , "Data.Either.lefts []"
                               , "Data.Either.rights []"
                               , "Data.Maybe.catMaybes []"
                               , "Data.Maybe.maybeToList Data.Maybe.Nothing"
                               , "GHC.Maybe.listToMaybe []" -- == Data.Maybe.Nothing
                               , "GHC.List.zipWith (,)" -- == GHC.List.zip
                              ]
      guard $ all (not . (`isInfixOf` showProg)) uselessSubPrograms      

      progSize <- sizeOfProg prog
      guard (progSize == sizeQuota) 

    -- | Using the components in env, like ("length", <a>. [a] -> Int)
    -- | tries to instantiate each, replacing type vars in order to unify with goalType
    getUnifiedComponent :: TopDownSolver IO (Id, SType)
    getUnifiedComponent = do
      (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)

      -- replaces "a" "b" with "tau1" "tau2"
      freshVars <- ourFreshType (env ^. boundTypeVars) schema "tau"

      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType

      assign isChecked True
      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      sub <- use typeAssignment

      let subbedType = stypeSubstitute sub (shape freshVars)

      guard =<< use isChecked
      return (id, subbedType)

      where
        -- moves all Data.Function functions to the end and moves the args to the front
        reorganizeSymbols :: [(Id, RSchema)]
        reorganizeSymbols = args ++ withoutDataFunctions

        ogSymbols            = Map.toList $ env ^. symbols
        (args, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) ogSymbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- | Replace all bound type variables with fresh free variables
    ourFreshType :: (CheckMonad (t m), MonadIO m) => [Id] -> RSchema -> Id -> t m RType
    ourFreshType bounds t id = ourFreshType' Map.empty [] t
      where
        ourFreshType' subst constraints (ForallT a sch) = do
            a' <- freshId bounds id
            ourFreshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
        ourFreshType' subst constraints (Monotype t) = return (typeSubstitute subst t)










-- extra stuff that is annoying me

printSub :: (MonadIO m) => StateT CheckerState m ()
printSub = do
  liftIO $ printf "sub = {\n"
  sub <- use typeAssignment
  liftIO $ mapM_ (\(id, t) -> printf "\t%s ==> %s (size %d)\n" id (show t) (sizeOfType t)) (Map.toList sub) --- * tau ==> Int
  subSize <- sizeOfSub
  liftIO $ printf "      } (size %d)\n\n" (subSize)



-- a wrapper for synthesize that you can run from `stack ghci`
-- usage: syn "Int -> Int"
-- or:    program <- syn "Int -> Int"
syn :: String -> IO ()
syn inStr = syn' inStr []
-- What if we can pass the guardInclude list into here??? :D
  
-- default search params when calling syn
searchP :: SearchParams
searchP = defaultSearchParams {_topDownEnableDebug = False, _topDownUseMemoize = False}


synGuard :: String -> [String] -> IO ()
synGuard inStr guards = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  let rawSyms = Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols
  goal <- envToGoal (env { _symbols = rawSyms}) inStr
  solverChan <- newChan
  synthesize searchP goal [] solverChan

-- usage:
-- :{
-- syn' "(a -> b) -> (a -> c) -> a -> (b, c)" $ [
--   [ (["\\x -> x + 1", "\\x -> x * 3", "3"], "(4, 9)")
--   , (["\\x -> x ++ x", "Data.List.reverse", "[1,2,3]"], "([1,2,3,1,2,3], [3,2,1])")
--   ]
-- :}
syn' :: String -> [([String], String)] -> IO ()
syn' inStr ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  goal <- envToGoal env inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize searchP goal examples solverChan

synGuard' :: String -> [String] -> [([String], String)] -> IO ()
synGuard' inStr guards ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  let rawSyms = Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols
  goal <- envToGoal (env { _symbols = rawSyms}) inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize searchP goal examples solverChan


data SynMode = IMode | EMode deriving (Eq, Ord, Show)
-- MemoMap: (mode, query, quota, args, sub) ==> (program), sub
type MemoMap = Map (SynMode, RType, Int, Map Id RType, Map Id SType) (Logic (RProgram, Map Id SType))
type TopDownSolver m = StateT CheckerState (StateT GoalTrace (LogicT (StateT MemoMap m)))

evalTopDownSolver :: Monad m => RType -> Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolver goalType messageChan m =
  (`evalStateT` Map.empty) $ observeT $ msum $ map (g . f) m
  where
    f = (`evalStateT` emptyChecker {_checkerChan = messageChan}) -- for StateT CheckerState
    g = (`evalStateT` [mkHole goalType])                         -- for StateT GoalTrace

-- | convert to Logic a
choices :: (Traversable f, MonadPlus m) => f a -> m a
choices = msum . fmap return

