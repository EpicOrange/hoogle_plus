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
import Data.Maybe (fromJust)
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

    -- call dfs with iterativeDeepening
    program <- iterativeDeepening env messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)

data SynMode = IMode | EMode deriving (Eq, Ord, Show)
data MemoKey = MemoKey {
    _mode :: SynMode,
    _goalType :: RType,
    _quota :: Int,
    _args :: Map Id RType,
    _sub :: Map Id SType
  } deriving (Eq, Ord)
-- MemoMap:        MemoKey ==> stream of (program,  sub)
type MemoMap = Map MemoKey     (Logic    (RProgram, Map Id SType))
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

-- evaluate runs `compute` until completion so it can have all of the results
-- evaluate adds one program at a time to the memo map ????
-- problem with adding one at a time: while calculating a goal T, if you later
--   encounter goal T again, then you will return a partial result
-- problem with using a flag to mark incomplete memo map items:
--   what do you do when the flag says incomplete??
--   do you double add? do you recompute and ignore memoization?
{-
  -----
  New way!!! store it at the actual size of the program instead of the quota
  like the bank for bottom up
  -----

  (?? :: T1 at quota 3)
    <- lookup (3, T1)   from the old map
    f (?? :: T2 at quota 2)
      a :: T2      quota 2
        send (f a) through check
          (1, T2) ==> [a] (incomplete)
          (2, T1) ==> [f a] (incomplete)
      b :: T2      quota 2
        send (f b) through check
          (1, T2) ==> [a, b] (incomplete)
          (2, T1) ==> [f a, f b] (incomplete)    <- we don't want to redo this work!!
      mzero
        mark T2 at quota 2 complete here
          (1, T2) ==> [a, b] (complete)
          (2, T2) ==> [] (complete)
    g (?? :: T1 at quota 2)
      we see (1, T1), (2, T1), and we see
      (2, T1) ==> [f a, f b] (incomplete)    <- we don't want to redo this work!!
      -- just keep going, ignoring the memo map
      f (?? :: T2 at quota 1)
        (1, T2) ==> [a, b] (complete)
        (2, T1) ==> [f a, f b] (incomplete)   <- overwrite existing
          [f a, f b] <- return this, but it's incomplete
            so we will re-evaluate everything and get:
          f a <- don't return it (guard)
          f b <- don't return it (guard)
          g (f a) <- add and return it   (hypothetically, but doesn't happen in this example)
        (2, T1) ==> [f a, f b] (incomplete)
        mzero
          mark T2 at quota 1 complete here
            (this is redundant)
      g (?? :: T1 at quota 1)
        mzero
          mark T1 at quota 1 as complete here
            (1, T1) ==> [] (complete)
      mzero
        mark T1 at quota 2 as complete here
          (2, T1) ==> [f a, f b] (complete)
          <-------
    mzero
      mark T1 at quota 3 as complete here
        (2, T1) ==> [f a, f b, g (f a), g (f b)] (complete)
-}

{-

==============
  PSEUDOCODE
==============


Map Key ([(RProgram, Sub)], Bool)
key = 
    change quota to be actual size of program
we have a goal and quota! lookup the key with size (1..quota-1)
  we get 
    Just list -> do
        (prog, sub) <- choices list
        return prog and update sub
    Nothing ->
        keep going to the next size
if we reach here, then we know that nothing of size 1..quota-1 was any good. 
  we lookup @ size quota
  Just (list, flag) ->
      (prog, sub) <- choices list
      return prog and update sub
  Nothing ->
      -- (will reinumerate all programs)
      prog <- compute `mplus` do
        map (1...quota) complete
      store prog, sub <---- important: add to the existing list (make sure it's not already there)
      return prog

Int -> Int
we start: Int (quota 2)
arg0

T (quota 3)

    sfsdfsfe
    
we're done
(3, T) ==> complete!
(2, T) ==> complete!
(1, T) ==> complete!

-}




memoizeProgram :: SynMode -> Int -> Map Id RType -> RType -> TopDownSolver IO RProgram -> TopDownSolver IO RProgram
memoizeProgram = undefined

memoizeProgramOld :: SynMode -> Int -> Map Id RType -> RType -> TopDownSolver IO RProgram -> TopDownSolver IO RProgram
-- memoizeProgram _ _ _ _ compute = compute
memoizeProgramOld mode quota args goalType compute = do
  st <- get -- TODO do this, but with 'use' and 'assign'
  memoMap <- lift $ lift $ get :: TopDownSolver IO MemoMap
  memoizeProgram' st memoMap
  where
    memoizeProgram' :: CheckerState -> MemoMap -> TopDownSolver IO RProgram
    memoizeProgram' st memoMap = case Map.lookup key memoMap of
      Just progs -> retrieve progs -- retrieve stored value
      Nothing    -> do

        evaluate                                      -- compute and store value
        -- then retrieve returns that as a stream
        retrieve $ fromJust $ Map.lookup key memoMap  -- retrieve stored value
      
      where
        sub = st ^. typeAssignment
        key = MemoKey mode goalType quota args sub
        
        -- after getting a stream of (program, sub) from the memo map,
        -- consume each value of the stream
        -- by adding the stored sub to the environment and then returning the program
        retrieve :: Logic (RProgram, Map Id SType) -> TopDownSolver IO RProgram
        retrieve progs = do
          (prog, savedSub) <- choices progs
          -- append the saved sub to our current and now updated sub
          let sub'' = Map.map (stypeSubstitute savedSub) sub <> savedSub
          -- update the current state's sub
          assign typeAssignment sub''
          return prog
        
        -- we can only add to memo map after we're completely done with compute
        -- so we run and return compute as normal, and then store the result when it runs out
        evaluate :: TopDownSolver IO RProgram
        evaluate = do
          -- compute `mplus` 
            -- get `compute` into a type we'll store into the memo map, i.e. Logic (RProgram, Map Id SType)
            let resultToStored (prog, checkerState) = (prog, checkerState ^. typeAssignment)
            st' <- get
            goalTrace <- lift get
            -- unwrap `compute` until it's a stream of StateT MemoMap IO [(RProgram, Map Id SType)]
            let x = fmap resultToStored $ (`evalStateT` goalTrace) $ (`runStateT` st) compute :: LogicT (StateT MemoMap IO) (RProgram, Map Id SType)
            -- lift until we get to the StateT MemoMap monad
            lift $ lift $ lift $ do
              -- consume the stream and store all the results into the memo map
              xs <- observeAllT x :: StateT MemoMap IO [(RProgram, Map Id SType)]
              let stored = choices xs
              modify (Map.insert key stored)
              mzero
            
-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver goalType messageChan $ (`map` [2,4..]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota

  -- plotted our tests, and solutions tend to have sub size = 3.7 * program size
  -- tests say (sub quota = 3 * program size quota) is best
  solution <- dfs EMode env Map.empty messageChan searchParams quota {-(quota * 3)-} goalType :: TopDownSolver IO RProgram
  lift $ modify (\goalTrace -> Symbol (show solution) : goalTrace) -- append solution to trace

  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution)
  
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


printSub :: (MonadIO m) => StateT CheckerState m ()
printSub = do
  liftIO $ printf "sub = {\n"
  sub <- use typeAssignment
  liftIO $ mapM_ (\(id, t) -> printf "\t%s ==> %s (size %d)\n" id (show t) (sizeOfType t)) (Map.toList sub) --- * tau ==> Int
  subSize <- sizeOfSub
  liftIO $ printf "      } (size %d)\n\n" (subSize)
  

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
dfs :: SynMode -> Environment -> Map Id RType -> Chan Message -> SearchParams -> Int {- -> Int-} -> RType -> TopDownSolver IO RProgram
dfs mode env args messageChan searchParams sizeQuota {-subQuota-} goalType 
  | sizeQuota <= 0 = mzero
  | useMemoize     = error "broken, don't use memoize" -- memoizeProgram mode sizeQuota args goalType doDfs
  | otherwise      = doDfs
  where

    -- | does DFS without memoization
    doDfs :: TopDownSolver IO RProgram
    doDfs = do
      debug $ printf "args to dfs: %s\n" (show $ (mode, args, sizeQuota, goalType))
      when enableDebug $ lift printGoalTrace
      -- holedProgram <- head <$> lift get
      -- debug $ printf "%s -> %s\n" (show goalType) (show holedProgram)
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

        -- add argument to new env and call dfs IMode with that new env
        let env' = addVariable argName tArg $ addArgument argName tArg env
        let args' = Map.insert argName tArg args

        -- we're synthesizing the body for the lambda
        -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
        lift $ addLam argName (show tArg)
        body <- dfs IMode env' args' messageChan searchParams (sizeQuota - 1) {-subQuota-} tBody

        let program = Program { content = PFun argName body, typeOf = goalType }
        progSize <- sizeOfProg program
        guard (progSize <= sizeQuota)

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

      -- need to save the last trace program so it replaces the right hole after the first dfs
      holedProgram <- head <$> lift get
      lift $ addApp (show schema) (show alpha)
      
      -- subtract 2; we may be setting the quota too low if alphaTProgram happens to be size 1,
      -- but that's rare, and we'll solve it in the next iteration anyways
      -- the time savings are worth it
      alphaTProgram <- dfs EMode env args messageChan searchParams (sizeQuota - 2) {-subQuota-} schema :: TopDownSolver IO RProgram
      -- debug $ printf "here!! %s\n" (show alphaTProgram)
      -- holedProgram <- head <$> lift get
      -- debug $ printf "-----\ntypeOf alphaTProgram (%s) = %s\n      %s\n" (show alphaTProgram) (show $ typeOf alphaTProgram) (show holedProgram)

      sub <- use typeAssignment
      let alphaSub = addTrue $ stypeSubstitute sub (shape alpha) :: RType
      
      lift $ addAppFilled alphaTProgram (show alpha) holedProgram

      alphaProgram <- dfs IMode env args messageChan searchParams (sizeQuota - sizeOfContent alphaTProgram) {-subQuota-} alphaSub :: TopDownSolver IO RProgram
      -- holedProgram <- head <$> lift get
      -- debug $ printf "typeOf alphaProgram (%s) = %s\n      %s\n" (show alphaProgram) (show $ typeOf alphaProgram) (show holedProgram)
      -- debug $ printf "--------------------\n"
      -- holedProgram <- head <$> lift get
      -- debug $ printf "current trace (remaining quota: %d): %s\n" (sizeQuota) (show holedProgram) 

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
      guard (progSize <= sizeQuota) 
      -- subSize <- sizeOfSub
      -- guard (subSize <= {-subQuota-})


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
      topDownSolveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      sub <- use typeAssignment

      let subbedType = stypeSubstitute sub (shape freshVars)
      -- debug $ printf "quota %d %d, (id, schema): %s :: %s\n\tt1: %s\n\tt2: %s\n\tinto: %s\n\tchecks: %s\n\n"
      --   sizeQuota {-subQuota-} id (show schema) (show t1) (show t2) (show $ subbedType) (show checkResult)
      
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
