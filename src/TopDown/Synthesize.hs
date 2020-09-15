{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal) where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import TopDown.Types
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
import qualified Data.Map.Merge.Strict as Map
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

    -- call dfs with iterativeDeepening
    program <- iterativeDeepening env messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)

memoizeProgram :: SynMode -> Int -> Map Id RType -> RType -> TopDownSolver IO (RProgram, Int) -> TopDownSolver IO (RProgram, Int)
memoizeProgram mode quota args goalType compute = do

  -- debug
  when False $ do
    memoMap <- liftMemo get
    liftIO $ printf "========quota %d=========\n" quota
    liftIO $ printMemoMap memoMap
    liftGoalTrace $ printGoalTrace
    printSub
    liftIO $ printf "========================\n"

  msum $ map lookupProg [1..quota]
  where
    lookupProg :: Int -> TopDownSolver IO (RProgram, Int)
    lookupProg num = do
      sub <- use typeAssignment
      let subbedGoal = addTrue $ stypeSubstitute sub (shape goalType)
      let key = MemoKey mode subbedGoal num args
      -- memoMap <- liftMemo get
      memoMap <- liftMemo get
      case Map.lookup key memoMap of
        -- found some stored programs, so return them
        Just stored           -> retrieve stored
        -- found no stored programs, but it's at a smaller quota so we keep checking
        Nothing | num < quota -> mzero
        -- found no stored programs, so we need to run compute @ quota to generate them
        Nothing               -> evaluate key

    retrieve :: MemoValue -> TopDownSolver IO (RProgram, Int)
    retrieve (list, isComplete) = do
      sub <- use typeAssignment
      -- liftIO $ when (not isComplete) error "oh no! isComplete is false... this shouldn't ever happen" 
      guard isComplete
      -- liftIO $ printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! we are using it yay! with goal: %s, got: %s\n" (show goalType) (show list)
      ourMap <- liftMemo get
      -- liftGoalTrace $ printGoalTrace
      -- liftIO $ printf "!!!!!!!!!!!  (mapSize: %d)\twe are using it yay! with (goal, quota): (%s, %s)\n" (Map.size ourMap) (show goalType) (show quota) 
      -- update the current state's sub

              -- 1. add together sub and storedSub (we already have code for this)
              --       don't do any substitutions. just add them together #simple

      (prog, subSize, savedSub, storedNameCounter) <- choices list
      -- append the saved sub to our current and now updated sub
      -- let sub' = savedSub <> Map.map (stypeSubstitute savedSub) sub
      -- let sub' = savedSub <> sub
      
      let s1 = sub
      let s2 = savedSub
      -- the reason this exists (rather than say, let sub' = s1 <> s2)
      -- is to make sure we don't load a component from memoize whose type conflicts with the current sub
      -- example 1: goal is b, sub is empty
      --   and we load (arg1 :: alpha0) with alpha0 ==> b  --- OK
      -- example 2: goal is b, sub has alpha0 ==> Maybe tau1
      --   and we load (arg1 :: alpha0) with alpha0 ==> b  --- NOT OK
      
      -- this is Map.unionWith, but takes in a monadic combining function
      let unionWithM f = Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithMaybeAMatched f)
      -- when the type variable k (e.g. alpha0) maps to both t1 and t2,
      -- unify k with t2 (the typeAssignment already has k ==> t1)
      -- guard isChecked
      -- return new unified value of k (e.g. alpha0)
      let combiner :: Id -> SType -> SType -> TopDownSolver IO (Maybe SType)
          combiner k t1 t2 = do
            unify undefined k t2
            guard =<< use isChecked
            Map.lookup k <$> use typeAssignment
      sub' <- unionWithM combiner s1 s2
      
         
         -- TODO this is where we are and why we're stuck
         {-
                - query: synGuard "a -> b" ["maybe"]
                - problems: 
                    * having issues with sub, not sure what to store and how to combine afterwards when we retrive 
                    * the size calculations are wrong now (we are no longer removing all the subs and that's causing issues with cacluating size)
         -}
        


      when ((show goalType) == "(alpha0 -> Either (a) (b))") $ do
        liftIO $ printf "==========\n"
        progSize <- sizeOfProg prog subSize
        liftIO $ printf "size is!!!!! --> %s\n" (show progSize)
        liftIO $ printf "\t* sub: %s\n" (show sub)
        liftIO $ printf "\t* savedSub: %s\n" (show savedSub)
        liftIO $ printf "\t* sub' (should be the two above combined): %s\n" (show sub')
        
      
        --  * sub: fromList [("alpha1",Either (tau2) (tau1)),("alpha2",tau1 -> alpha0 -> Either (a) (b)),("alpha3",tau2 -> alpha0 -> Either (a) (b))]
        --  * savedSub: fromList [("alpha0",Either (a) (b))]
        --  * sub' (should be the two above combined): fromList [("alpha0",Either (a) (b)),("alpha1",Either (tau2) (tau1)),("alpha2",tau1 -> Either (a) (b) -> Either (a) (b)),("alpha3",tau2 -> Either (a) (b) -> Either (a) (b))]

      assign typeAssignment sub'
      assign nameCounter storedNameCounter
      return (prog, subSize)
    
     
    -- runs compute, storing every program as it goes,
    -- and at the end, sets isComplete to true
    evaluate :: MemoKey -> TopDownSolver IO (RProgram, Int)
    evaluate key = do
        -- (will reenumerate all programs and possibly change memoMap)
        beforeSub <- use typeAssignment
        (prog, subSize) <- compute `mplus` setComplete key
        afterSub <- use typeAssignment
        when (show goalType == "(alpha0 -> b)") $ do
          liftIO $ printf "\n==================== !!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
          liftGoalTrace $ printGoalTrace

 
        -- take the difference between the subs
        -- let subDiff = afterSub `Map.difference` beforeSub

        -- 1. filter everything in sub'
        --       only keep type variables that are used in the query
        -- 2. store updatedSub in the map
        --    store nameCounter in the map

        -- only keeps the parts of the new sub that are relevant to the query
        --   TODO this is problematic:
        --   when we solve (?? :: b) with component (arg1 :: alpha0),
        --   this erases the alpha0 ==> b in sub
        let updatedSub = afterSub `Map.difference` beforeSub
        -- let updatedSub = Map.filterWithKey (\k v -> k `isInfixOf` (show goalType)) afterSub
        -- when ("alpha0" `elem` (Map.keys afterSub) && not ("alpha0" `elem` (Map.keys updatedSub))) $ do
        --   liftIO $ do
        --     printf "\n====================\nFuck! We removed alpha0 from goalType %s\n" (show goalType)
        --     printf "afterSub: %s\n" (show afterSub)
        --     printf "updatedSub: %s\n" (show updatedSub)
            -- error "damn"

-- Fuck! We removed alpha0 from goalType (tau2 -> Either (a) (b))
-- Fuck! We removed alpha0 from goalType (Either (a) (b) -> Either (a) (b))
-- "alpha0",Either (tau2) (tau1)
        
        -- idea: maybe we shouldn't check just the goal but also the sub
        -- fromList [("alpha1",Either (tau2) (tau1)),("alpha2",tau1 -> alpha0 -> Either (a) (b)),("alpha3",tau2 -> alpha0 -> Either (a) (b))]

        -- when ((show $ _goalType key) == "(alpha1 -> [b])" || (show $ _goalType key) == "(alpha0 -> [b])") $ do
        --   liftIO $ printf "program: %s, beforeSub: %s\n" (show prog) (show beforeSub)
        --   liftIO $ printf "program: %s, afterSub: %s\n" (show prog) (show afterSub)
        --   liftIO $ printf "program: %s, subDiff: %s\n" (show prog) (show subDiff)
        
        -- TODO add these comments to the parts of the code that they apply to (so that it breaks up the massive comment)
        -- we want to add this prog to the existing memo map if possible
        -- there's 3 situations
        --   1. key is not in the map
        --      -> we want to add and return prog
        --   2. (key ==> list) is in the map, and prog is already in the list
        --      -> we want to not return prog (do nothing) --- guard
        --   3. (key ==> list) is in the map, and prog is not in the list
        --      -> we want to append and return prog

        progSize <- sizeOfProg prog subSize
        progNameCounter <- use nameCounter
        memoMap' <- liftMemo get :: TopDownSolver IO MemoMap
        -- sub <- use typeAssignment
        
        let subbedGoal = addTrue $ stypeSubstitute afterSub (shape goalType)
        let key' = MemoKey mode goalType progSize args
        let storedValue = (prog, subSize, updatedSub, progNameCounter)
        
        let add :: RProgram -> MemoMap
            add prog = do
              let f Nothing                   = Just ([storedValue], False)
                  f (Just (list, isComplete)) = Just (storedValue:list, isComplete)
              Map.alter f key' memoMap'
        
        case Map.lookup key' memoMap' of
          Just (list, isComplete) -> do
            -- see if there's an existing (prog, sub) in the list (ignoring nameCounter)
            -- if so, update the nameCounter to be the maximum of itself and progNameCounter
            case find (\(p,s,u,c) -> (p,s,u) == (prog, subSize, updatedSub)) list of
              -- if it isn't in the completed list, that's wrong because isComplete means everything's in the list
              Nothing | isComplete -> do
                memoMap <- liftMemo get
                liftIO $ printMemoMap memoMap
                liftGoalTrace $ printGoalTrace
                error $ printf "oops... (%s) %s @ size %s says complete but isn't there: \n\t%s \n\tsub: %s\n\tnameCounter: %s\n\targs: %s\n\tafterSub: %s\n\tbeforeSub: %s\n" 
                  (show mode) (show goalType) (show progSize) (show prog) (show $ Map.toList updatedSub) (show $ Map.toList progNameCounter) (show $ args)
                  (show $ afterSub) (show $ beforeSub)
              -- if it isn't in the list but the list is incomplete, add it
              Nothing      -> do
                liftMemo $ put $ add prog
                -- liftIO $ printf "\t### adding program %s to (size %d) goal %s\n" (show prog) (quota) (show goalType)
                return (prog, subSize)
              -- if (prog, sub) is already in the list, update the nameCounter to be the maximum of itself and progNameCounter
              Just (_,_,_,c) -> do
                let list' = map (\(p,s,u,c) -> if (p,s,u) == (prog, subSize, updatedSub) then (p, s, u, Map.unionWith max c progNameCounter) else (p,s,u,c)) list
                liftMemo $ put $ Map.insert key' (list', isComplete) memoMap'
                return (prog, subSize)

            -- let isInList =  :: Bool -- check if  is in list
            -- when (not isInList && isComplete) $ do

            -- guard (not isInList) -- assert we're not already in the list
            -- add the program to our memo map!
          Nothing           -> do
            -- add the program to our memo map!
            liftMemo $ put $ add prog
            -- liftIO $ printf "\t### adding program %s to (size %d) goal %s\n" (show prog) (quota) (show goalType)
            return (prog, subSize)
          
    -- | set the complete flag and return mzero, called when we're done with compute
    setComplete :: MemoKey -> TopDownSolver IO (RProgram, Int)
    setComplete ogKey = do
      let ogSize = _progSize ogKey
      -- when (show (_goalType ogKey) == "(alpha1 -> [(b , c)])") $
          -- liftIO $ printf "-------------\nmarking %s complete for size up to %d\n-------------\n" (show $ _goalType ogKey) (_progSize ogKey)
      -- mark complete every entry matching this description:
      --   (ignoring size) key is identical to ogKey AND size of key is less than quota
      let markComplete key (list, isComplete)
            | key {_progSize = ogSize} == ogKey && _progSize key <= quota = (list, True)
            | otherwise     = (list, isComplete)
      -- map [every value with size=(1...quota)] complete
      -- liftIO $ printf "\t### we're marking key %s @ size <= %d as complete\n" (show $ _goalType ogKey) quota
      liftMemo $ modify $ Map.mapWithKey markComplete
      mzero

-- 
-- | try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver goalType messageChan $ (`map` [1..]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota

  -- plotted our tests, and solutions tend to have sub size = 3.7 * program size
  -- tests say (sub quota = 3 * program size quota) is best
  (solution, subSize) <- dfs EMode env Map.empty messageChan searchParams quota {-(quota * 3)-} goalType :: TopDownSolver IO (RProgram, Int)
  liftGoalTrace $ modify (\goalTrace -> Symbol (show solution) : goalTrace) -- append solution to trace

  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution)
  
  -- call check on the program
  guard =<< liftIO (check' solution)

  -- subSize <- sizeOfSub
  -- subSize <- liftSubSize get
  -- when enableDebug $ liftGoalTrace printGoalTrace
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
dfs :: SynMode -> Environment -> Map Id RType -> Chan Message -> SearchParams -> Int {- -> Int-} -> RType -> TopDownSolver IO (RProgram, Int)
dfs mode env args messageChan searchParams sizeQuota {-subQuota-} goalType
  | sizeQuota <= 0 = mzero
  | otherwise     = do
    -- make sure the sub isn't too big before proceeding 
    -- subSize <- sizeOfSub
    -- subSize <- liftSubSize get
    -- guard (subSize < sizeQuota)
    -- TODO since getting rid of subSize stuff, is that ok? 
    if useMemoize
      then memoizeProgram mode sizeQuota args goalType doDfs
      else doDfs

  where

    -- | does DFS without memoization
    doDfs :: TopDownSolver IO (RProgram, Int)
    doDfs = do
      -- debug $ printf "args to dfs: %s\n" (show $ (mode, args, sizeQuota, goalType))
      -- when enableDebug $ lift printGoalTrace
      -- holedProgram <- head <$> lift get
      -- debug $ printf "%s -> %s\n" (show goalType) (show holedProgram)
      memoMap <- liftMemo get :: TopDownSolver IO MemoMap
      (prog, subSize) <- inEnv `mplus` doSplit mode
      progSize <- sizeOfProg prog subSize
      guardCheck prog subSize

      -- debug $ printf "right before returning2: \n"
      -- printSub

      return (prog, subSize)

    -- search params
    useAltIMode = _topDownUseAltIMode searchParams
    useMemoize  = _topDownUseMemoize searchParams
    enableDebug = _topDownEnableDebug searchParams

    -- | only prints things when we've enabled debugging
    debug :: (MonadIO m) => IO () -> m ()
    debug m = when enableDebug $ liftIO m
    
    -- | return components whose entire type unify with goal type
    inEnv :: TopDownSolver IO (RProgram, Int)
    inEnv = do
      -- only check env if e mode, or if we're using the alt I mode
      guard (useAltIMode || mode == EMode)
      (id, schema, subSize) <- getUnifiedComponent :: TopDownSolver IO (Id, SType, Int)
      return (Program { content = PSymbol id, typeOf = addTrue schema }, subSize)

    -- | add args to env, and search for the return type
    doSplit :: SynMode -> TopDownSolver IO (RProgram, Int)
    doSplit IMode = case goalType of
      ScalarT _ _            -> doSplit EMode
      FunctionT _ tArg tBody -> do
        argName <- freshId (Map.keys $ env ^. arguments) "arg"

        -- add argument to new env and call dfs IMode with that new env
        let env' = addVariable argName tArg $ addArgument argName tArg env
        let args' = Map.insert argName tArg args

        -- we're synthesizing the body for the lambda
        -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
        -- sub <- use typeAssignment
        -- let subbedArg = stypeSubstitute sub (shape tArg)
        -- debug $ printf "========\n"
        -- printSub
        -- debug $ printf "========\n"
        -- liftGoalTrace $ addLam argName (show subbedArg)
        liftGoalTrace $ addLam argName (show tBody)
        (body, subSize) <- dfs IMode env' args' messageChan searchParams (sizeQuota - 1) {-subQuota-} tBody

        let program = Program { content = PFun argName body, typeOf = goalType }
        progSize <- sizeOfProg program subSize
        guard (progSize <= sizeQuota)
        -- debug $ printf "right before returning: \n" -- doesn't have alpha0
        -- printSub

        return (program, subSize)

    -- we split (?? :: T) into (??::alpha -> T) (??::alpha)
    doSplit EMode = do
      when (show goalType == "(alpha1 -> [(b , c)])") $ debug $ printf "\n------\nHERE IN SPLIT\n"
      let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
      let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema
      
      -- need to save the name counter so it generates the same tau for each ourFreshType call
      nameCtr <- getNameCounter
      alpha <- ourFreshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') "alpha" :: TopDownSolver IO RType
      setNameCounter nameCtr
      schema <- ourFreshType (env ^. boundTypeVars) schema' "alpha" :: TopDownSolver IO RType

      -- need to save the last trace program so it replaces the right hole after the first dfs
      holedProgram <- head <$> liftGoalTrace get
      liftGoalTrace $ addApp (show schema) (show alpha)
      
      -- subtract 2; we may be setting the quota too low if alphaTProgram happens to be size 1,
      -- but that's rare, and we'll solve it in the next iteration anyways
      -- the time savings are worth it
      -- alphaTProgram <- dfs EMode env args messageChan searchParams (sizeQuota - 2) {-subQuota-} schema :: TopDownSolver IO RProgram
      (alphaTProgram, alphaTSubSize) <- dfs EMode env args messageChan searchParams (sizeQuota - 1) {-subQuota-} schema :: TopDownSolver IO (RProgram, Int)
      -- alphaTProgram <- dfs EMode env args messageChan searchParams (sizeQuota) {-subQuota-} schema :: TopDownSolver IO RProgram
      -- debug $ printf "here!! %s\n" (show alphaTProgram)
      -- holedProgram <- head <$> lift get
      -- debug $ printf "-----\ntypeOf alphaTProgram (%s) = %s\n      %s\n" (show alphaTProgram) (show $ typeOf alphaTProgram) (show holedProgram)

      sub <- use typeAssignment
      let alphaSub = addTrue $ stypeSubstitute sub (shape alpha) :: RType
      
      liftGoalTrace $ addAppFilled alphaTProgram (show alpha) holedProgram

      -- (sizeQuota == 6) here???????
      alphaTSize <- sizeOfProg alphaTProgram alphaTSubSize
      let alphaQuota = sizeQuota - alphaTSize
      when (show alphaTProgram == "GHC.List.zip") $ do
        debug $ printf "****************** sizeQuota: %d, alphaQuota: %d, sizeOfContent (%s) = %d\n" sizeQuota alphaQuota (show alphaTProgram) (sizeOfContent alphaTProgram)
        printSub
      
      (alphaProgram, alphaSubSize) <- dfs IMode env args messageChan searchParams alphaQuota {-subQuota-} alphaSub :: TopDownSolver IO (RProgram, Int)
      
      when (show alphaTProgram == "GHC.List.zip") $ do
        debug $ printf "****************** alphaProgram: %s, size: %d\n" (show alphaProgram) (sizeOfContent alphaProgram)
        printSub

      -- holedProgram <- head <$> liftGoalTrace get
      -- debug $ printf "typeOf alphaProgram (%s) = %s\n      %s\n" (show alphaProgram) (show $ typeOf alphaProgram) (show holedProgram)
      -- debug $ printf "--------------------\n"
      -- holedProgram <- head <$> liftGoalTrace get
      -- debug $ printf "current trace (remaining quota: %d): %s\n" (sizeQuota) (show holedProgram) 
      return (Program {
          content = case content alphaTProgram of
                      PSymbol id -> PApp id [alphaProgram]
                      PApp id xs -> PApp id $ xs ++ [alphaProgram],
          typeOf = goalType
        }, alphaTSubSize + alphaSubSize)

    -- | guards away programs that we don't want
    guardCheck :: RProgram -> Int -> TopDownSolver IO ()
    guardCheck prog subSize = do
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

      progSize <- sizeOfProg prog subSize
      guard (progSize <= sizeQuota) 
      -- subSize <- sizeOfSub
      -- guard (subSize <= {-subQuota-})


    -- | Using the components in env, like ("length", <a>. [a] -> Int)
    -- | tries to instantiate each, replacing type vars in order to unify with goalType
    getUnifiedComponent :: TopDownSolver IO (Id, SType, Int)
    getUnifiedComponent = do
      (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)

      -- replaces "a" "b" with "tau1" "tau2"
      freshVars <- ourFreshType (env ^. boundTypeVars) schema "tau"
      -- when ((show goalType) == "(alpha1 -> [b])" || (show goalType) == "(alpha0 -> [b])") $ debug $ printf "freshVars: %s\n" (show freshVars)
      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType

      assign isChecked True
      topDownSolveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      sub <- use typeAssignment

      let subbedType = stypeSubstitute sub (shape freshVars)
      -- debug $ printf "quota %d %d, (id, schema): %s :: %s\n\tt1: %s\n\tt2: %s\n\tinto: %s\n\tchecks: %s\n\n"
      --   sizeQuota {-subQuota-} id (show schema) (show t1) (show t2) (show $ subbedType) (show checkResult)
      guard =<< use isChecked

      {-
        1. sum up all the taus (sizeOfSub)
        2. add that to current subSize
        3. substitute all free variables in sub
        4. remove all taus from sub
      -}
      -- subSize <- sizeOfSub
      -- liftSubSize $ modify (+subSize)
      -- sizeSub <- liftSubSize get
      -- debug $ printf "-------------\n"
      -- printSub
      
      
      {- TODO problem: 

            length :: a . [a]    -> Int
                          alpha -> Int
            sub = {
                    alpha0 ==> [tau0] (size 2)
                    tau0 ==> is missing!!!
                  } (size 0)

            GHC.List.length arg0 unified with subSize: 0, for total size: 0


(Quota 3) Done with <a> . ([a] -> Int)!
size +  subSize solution
2       1       GHC.List.length arg0

sub = {
        alpha0 ==> [a] (size 2)
      } (size 1)

        ---------

        here, there is no tau0 in map because it's a free varialbe. do we still want the subsize to be 0? 

        (alpha0 -> T) (alpha)
          (arg0, 5)     (arg1, 4)

          (arg0 arg1, 5 + 4 = 9)

      
        also, how do we isolate the size of the program? with just the things from the sub list that apply to the program at hand? 
        I think our method of just accumulating things is wrong... FUCK lol 
        I think we have to return the program size as a tuple from getUnifiedComponent and use that as the size to that program... 
        and always return (RProgram, Int) everywhere
        and when we're building up programs, combine their 2 sizes together
        and get that size from getUnifiedComponent
      
      -}



-- ===================
-- Here!!!
-- sub = {
--         alpha0 ==> Either (tau2) (Either (a) (b)) (size 5)
--         alpha1 ==> Either (a) (b) -> Either (a) (b) (size 6)
--         alpha2 ==> tau2 -> Either (a) (b) (size 4)
--         tau1 ==> Either (a) (b) (size 3)
--       } (size 0)
-- ===================

      addedSubSize <- sizeOfSub
      let sub' = Map.map (stypeSubstitute sub) sub
      let sub'' = Map.filterWithKey (\k v -> not $ "tau" `isPrefixOf` k) sub'
      

      assign typeAssignment sub''
      
      -- when (show goalType == "Either (a) (b)"  && id == "arg2") $ do
        
      --   debug $ printf "\n\n\n===================\nHere!!!\n"
      --   printSub --- this should include alpha0!!!!!! let's see if it does
      --   debug $ printf "sub'': %s\n" (show sub'')
      --   debug $ printf "===================\n"
      --   error "fuck"

      return (id, subbedType, addedSubSize)
      -- return (id, subbedType)

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