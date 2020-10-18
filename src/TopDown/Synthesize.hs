{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal) where

import Prelude hiding (log)
import TopDown.Debug
import TopDown.GoalTrace
import TopDown.Memoize
import TopDown.Size
-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import TopDown.Types
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
import Control.Monad.Reader
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

    let useHO = _useHO searchParams

    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates

     -- add higher order functions to the environment
    let newSyms = if useHO
        then Map.filterWithKey (\k a -> not $ "'ho'" `isInfixOf` k) rawSyms
        else Map.withoutKeys (Map.filter (not . isHigherOrder . toMonotype) rawSyms) $ Set.fromList hoCands
    let env = rawEnv { _symbols = newSyms, _hoCandidates = [] }
    let goal = shape $ lastType $ toMonotype goalType :: SType

    let enableDebug = _topDownEnableDebug searchParams

    when enableDebug $ do
      putStrLn "\n=================="
      putStrLn "Starting!"
      printf "Arguments:\n"
      mapM_ (\(id, t) -> printf "\t%s :: %s\n" id (show t)) $ Map.toList $ env ^. arguments
      printf "Goal: %s\n" (show goal)
      putStrLn "=================="

    -- run dfs
    program <- iterativeDeepening env messageChan searchParams examples goalType

    -- TODO remove this when we stop using messageChan
    writeChan messageChan (MesgClose CSNormal)

-- 
-- | try to get solutions by calling dfs on size 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver searchParams messageChan goalType $ (`map` [1..]) $ \quota -> do
  when (quota == 1) $ do
    log 0 $ "LOG\n===\n\n"
    log 0 $ "goal to synthesize:\n"
    log 1 $ show goal ++ "\n"
    log 0 $ "with components:\n"
    mapM_ (log 1) $ map (\(id, schema) -> printf "%s :: %s\n" id (show schema)) $ Map.toList $ env ^. symbols

  -- LOGGING STUFF
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota
  log 0 $ printf "\nQuota %d\n=========\n\n" quota
  ctr <- liftDebug $ use dfsCounter
  
  if ctr == 1 
    then logEcho $ printf "dfs has been entered %d time\n" ctr
    else logEcho $ printf "dfs has been entered %d times\n" ctr
  
  memoMap <- liftMemo get
  log 0 $ printf "memo map looks like:\n%s\n" (showMemoMap memoMap)
  -- liftIO $ printf "memo map looks like:\n%s\n" (showMemoMap memoMap)
  liftIO $ printMemoMap memoMap

  (prog, subSize) <- dfs EMode env searchParams goalType 0 quota :: TopDownSolver IO (RProgram, Int)
  guard (filterParams prog) -- see if we mention all args before we call check
  guard =<< liftIO (check' prog) -- run demand checker and example checker

  goalTrace <- liftGoalTrace get
  debugGoalTrace (Symbol (show prog) : goalTrace)
  logEcho $ printf "\n(Quota %d) Done with %s!\nsize + subSize solution\n%4d + %-7d %s\n\n" quota (show goal) (sizeOfContent prog) subSize (show prog)

  ctr <- liftDebug $ use dfsCounter
  if ctr == 1 
    then logEcho $ printf "dfs has been entered %d time\n" ctr
    else logEcho $ printf "dfs has been entered %d times\n" ctr
  
  return prog
  where
    goalType :: RType
    goalType = lastType $ toMonotype goal

    -- determines if the result has all the passed-in arguments
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

--
-- does DFS in either E-mode or I-mode
-- dfs EMode:
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
-- dfs IMode:
--    * checks if anything in the environment matches the full type of goal
--    * if is a function type, add args to env, search for the return type, and return a lambda 
--    * if not, search in e-mode
--
dfs :: SynMode -> Environment -> SearchParams -> RType -> Int -> Int -> TopDownSolver IO (RProgram, Int)
dfs mode env searchParams goalType depth quota
  | quota <= 0 = mzero
  --  | quota == 1 = do
  | useMemoize = memoizeProgram env mode quota goalType depth doDfs
  | otherwise  = doDfs
  where
    -- | does DFS without memoization
    doDfs :: TopDownSolver IO (RProgram, Int)
    doDfs = do
      liftDebug incrementDfsCounter
      
      (prog, subSize) <- if quota == 1 
          then inEnv 
          else doSplit mode

      -- (prog, subSize) <- do
      --   if quota == 1 
      --     then inEnv 
      --     else do
      --       if (useMemoize) 
      --         then do
      --           sub <- use typeAssignment
      --           let subbedGoal = addTrue . stypeSubstitute sub . shape $ goalType
      --           memoizeProgram mode quota subbedGoal depth doDfs
      --         else doSplit mode
      
      -- TODO remove later
      when (subSize /= 0) $ liftIO $ printf "nooo!!!! \n"
      
      -- progSize <- sizeOfProg prog subSize
      -- progSize <- sizeOfProg' prog 
      guardCheck prog subSize
      return (prog, subSize)

    -- | search params
    useAltIMode = _topDownUseAltIMode searchParams
    useMemoize  = _topDownUseMemoize searchParams
    -- enableDebug = _topDownEnableDebug searchParams

    -- | return components which unify with goal exactly
    inEnv :: TopDownSolver IO (RProgram, Int)
    inEnv = do
      -- only check env if e mode, or if we're using the alt I mode
      guard (useAltIMode || mode == EMode)
      (id, schema, t, subSize) <- getUnifiedComponent :: TopDownSolver IO (Id, RSchema, SType, Int)
      
      sub <- use typeAssignment
      
      -- check if the program we're returning is exactly of size == quota
      if (1+subSize == quota)
        then do
          log (depth+1) $ printf "unified (size %d): %s :: %s ~ %s via %s\n" (1+subSize) id (show schema) (show goalType) (showMap sub)
          return (Program { content = PSymbol id, typeOf = addTrue t }, subSize)
        else do
          log (depth+1) $ printf "skipped (size %d /= quota %d): %s :: %s ~ %s via %s\n" (1+subSize) quota id (show schema) (show goalType) (showMap sub)
          mzero

    -- | add args to env, and search for the return type
    doSplit :: SynMode -> TopDownSolver IO (RProgram, Int)
    doSplit IMode = case goalType of
      ScalarT _ _            -> doSplit EMode
      FunctionT _ tArg tBody -> do
        
        log (depth+1) $ printf "done with inEnv, split since quota (%d) > 1\n" quota
        existingArgs <- liftArgs get
        argName <- freshId (Map.keys $ env ^. arguments) "arg"

        -- introduce an arg
        liftArgs $ modify $ Map.insert argName tArg
        log (depth+2) $ printf "introducing %s :: %s as a component\n" argName (show tArg)

        -- synthesize the body for the lambda
        liftGoalTrace $ addLam argName (show tBody)
        (body, subSize) <- dfs IMode env searchParams tBody (depth + 2) (quota - 1)

        -- reset the args to before we synthesize this lambda body
        log (depth+2) $ printf "removing %s :: %s as a component\n" argName (show tArg)
        liftArgs $ put existingArgs

        let program = Program { content = PFun argName body, typeOf = goalType }
        
        -- progSize <- sizeOfProg program subSize
        -- progSize <- sizeOfProg program subSize
        -- guard (progSize == quota)

        return (program, subSize)

    -- | we split (?? :: T) into (??::alpha -> T) (??::alpha)
    doSplit EMode | quota <= 1 = do
      log (depth+1) $ printf "done with inEnv, won't split since quota is %d\n" quota
      mzero
    
    doSplit EMode = do
      log (depth+1) $ printf "done with inEnv, split since quota (%d) > 1\n" quota

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
      

      -- get programs that are up to quota: (quota-1)
      (alphaTProgram, alphaTSubSize) <- msum $ map (dfs EMode env searchParams schema (depth + 2)) [1..quota - 1] :: TopDownSolver IO (RProgram, Int)
      
      -- TODO infer the types from alphaTProgram to figure out what alphaProgram's goal should be
      
      sub <- use typeAssignment
      
      let alphaSub = addTrue $ stypeSubstitute sub (shape alpha) :: RType
      liftGoalTrace $ addAppFilled alphaTProgram (show alpha) holedProgram

      alphaTSize <- sizeOfProg alphaTProgram alphaTSubSize
      let alphaQuota = quota - alphaTSize
      
      (alphaProgram, alphaSubSize) <- dfs IMode env searchParams alphaSub (depth + 4) alphaQuota :: TopDownSolver IO (RProgram, Int)
      
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
      -- progSize <- sizeOfProg' prog 
      guard (progSize <= quota) 


    -- | Using the components in env, like ("length", <a>. [a] -> Int)
    -- | tries to instantiate each, replacing type vars in order to unify with goalType
    getUnifiedComponent :: TopDownSolver IO (Id, RSchema, SType, Int)
    getUnifiedComponent = do
      args <- liftArgs get :: TopDownSolver IO (Map Id RType)
      let args' = Map.toList $ Map.map Monotype args :: [(Id, RSchema)]
      (id, schema) <- choices $ args' ++ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)

      -- replaces "a" "b" with "tau1" "tau2"
      freshVars <- ourFreshType (env ^. boundTypeVars) schema "tau"
      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType

      assign isChecked True
      topDownSolveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      sub <- use typeAssignment

      let subbedType = stypeSubstitute sub (shape freshVars)
      guard =<< use isChecked

      addedSubSize <- if enableSubSize then sizeOfSub else return 0
      
      let sub' = Map.map (stypeSubstitute sub) sub
      liftArgs $ modify $ Map.map (addTrue . stypeSubstitute sub . shape)
      
      -- let sub'' = Map.filterWithKey (\k v -> not $ "tau" `isPrefixOf` k) sub'
      
      -- assign typeAssignment sub''
      assign typeAssignment sub'

      return (id, schema, subbedType, addedSubSize)

      where
        -- moves all Data.Function functions to the end and moves the args to the front
        reorganizeSymbols :: [(Id, RSchema)]
        reorganizeSymbols = argSymbols ++ withoutDataFunctions

        ogSymbols            = Map.toList $ env ^. symbols
        (argSymbols, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) ogSymbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- | Replace all bound type variables with fresh free variables
    -- | Our version lets you pass in the prefix (id)
    ourFreshType :: (CheckMonad (t m), MonadIO m) => [Id] -> RSchema -> Id -> t m RType
    ourFreshType bounds t id = ourFreshType' Map.empty [] t
      where
        ourFreshType' subst constraints (ForallT a sch) = do
            a' <- freshId bounds id
            ourFreshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
        ourFreshType' subst constraints (Monotype t) = return (typeSubstitute subst t)