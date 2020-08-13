{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, synGuard, synGuard', syn', sizeOf) where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import HooglePlus.GHCChecker (check)
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
syn :: String -> IO ()
syn inStr = syn' inStr []
-- What if we can pass the guardInclude list into here??? :D
  
synGuard :: String -> [String] -> IO ()
synGuard inStr guards = do
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
syn' :: String -> [([String], String)] -> IO ()
syn' inStr ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  goal <- envToGoal env inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize defaultSearchParams goal examples solverChan

synGuard' :: String -> [String] -> [([String], String)] -> IO ()
synGuard' inStr guards ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  let rawSyms = Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols
  goal <- envToGoal (env { _symbols = rawSyms}) inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize defaultSearchParams goal examples solverChan


envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> let e = toErrorMessage parseErr
                      in putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> do
            let (env', monospec) = updateEnvWithBoundTyVars sp env
            let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
            return $ goal { gEnvironment = env'', gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc linebreak >> error (prettyShow parseErr)
      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
synthesize searchParams goal examples messageChan = do
    
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal :: RSchema
    -- let destinationType = lastType (toMonotype goalType)
    -- let useHO = _useHO searchParams
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


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolver :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolver messageChan m =
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolver messageChan $ (`map` [1..]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota
  let goalType = lastType $ toMonotype goal :: RType

  solution <- dfsEMode env messageChan quota (quota * 3) goalType :: TopDownSolver IO RProgram
  
-- << sizeQuota 4, subQuota 14 >>: sizeOf (Data.Maybe.fromJust arg0 arg1) = 3, 6
--         * ("tau0",a)
--         * ("tau1",Maybe (a -> b))
--         * ("tau2",a -> b)

  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution env)
  subSize <- sizeOfSub
  liftIO $ printf "\nnew program: %s\n" (show solution) 
  liftIO $ printf "\nprogramSize: %d\n\tsubSize %d\n\n" (sizeOf solution) subSize
  -- liftIO $ printf "new program: %s\n" (show solution)


  
  -- call check on the program
  guard =<< liftIO (check' solution)

  return solution
  where

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
-- does DFS in I-mode
--    * if is a function type, split args out and return a lambda 
--    * if not, searched in e-mode 
--
dfsIMode :: Environment -> Chan Message -> Int -> Int -> RType -> TopDownSolver IO RProgram
dfsIMode env messageChan sizeQuota subQuota goalType 
  | sizeQuota <= 0 = mzero
  | otherwise = do
      -- subSize <- sizeOfSub
      -- guard (subSize > subQuota)
      
      case goalType of
        
        -- if function type, split up the arguments and add them to the environment
        FunctionT _ tArg tBody -> do
          
          -- TODO first check if things are in env, and then split up otherwise
          argName <- freshId (Map.keys $ env ^. arguments) "arg"

          -- add argument to new env and call dfsIMode with that new env
          let newEnv = addVariable argName tArg $ addArgument argName tArg env

          -- we're synthesizing the body for the lambda
          -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
          body <- dfsIMode newEnv messageChan (sizeQuota - 1) subQuota tBody

          let program = Program { content = PFun argName body, typeOf = goalType }

          -- liftIO $ printf "quota %d: sizeOf (%s) = %d\n" quota (show program) (sizeOf program)
          
          guard (sizeOf program <= sizeQuota)

-- (Data.Maybe.fromJust (Data.Maybe.Just (GHC.List.last (GHC.List.last (GHC.List.concat [])))) :: b) = 6, 1


          return program
        
        -- not a function type, switch into e-mode
        ScalarT _ _     -> do
          dfsEMode env messageChan sizeQuota subQuota goalType 
        
        _ -> error "unsupported goalType for dfsIMode"

--
-- does DFS in E-mode
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
--
dfsEMode :: Environment -> Chan Message -> Int -> Int -> RType -> TopDownSolver IO RProgram
dfsEMode env messageChan sizeQuota subQuota goalType 
  | sizeQuota <= 0 = mzero
  | otherwise      = do
      -- guard (subSize > subQuota)
      
      prog <- inEnv `mplus` doSplit
      filterBottomHack prog

      st <- get
      let sub = st ^. typeAssignment
      subSize <- sizeOfSub
      -- liftIO $ printf "<< sizeQuota %d, subQuota %d >>: sizeOf (%s) = %d, %d\n" sizeQuota subQuota (show prog) (sizeOf prog) (subSize)
      -- liftIO $ mapM_ (printf "\t* %s\n" . show) $ Map.toList sub
      -- liftIO $ printf "<< sizeQuota %d, subSize %d >>: sizeOf (%s :: %s) = %d\n" sizeQuota  (subSize) (show prog) (show $ typeOf prog) (sizeOf prog)
      guard (sizeOf prog <= sizeQuota)

-- quota 10: sizeOf (Data.Maybe.fromJust :: (Maybe (b) -> b)) = 4
-- quota 6: sizeOf (Data.Maybe.fromJust :: (Maybe ((Maybe (b))) -> Maybe (b))) = 6
-- quota 5: sizeOf (Data.Maybe.fromJust :: (Maybe (((tau2 -> Maybe (b)))) -> (tau2 -> Maybe (b)))) = 8
-- quota 4: sizeOf (Data.Maybe.fromJust :: (Maybe (((tau3 -> (tau2 -> Maybe (b))))) -> (tau3 -> (tau2 -> Maybe (b))))) = 10
-- quota 3: sizeOf (Data.Maybe.fromJust :: (Maybe (((tau4 -> (tau3 -> (tau2 -> Maybe (b)))))) -> (tau4 -> (tau3 -> (tau2 -> Maybe (b)))))) = 12
-- quota 2: sizeOf (Data.Maybe.fromJust :: (Maybe (((tau5 -> (tau4 -> (tau3 -> (tau2 -> Maybe (b))))))) -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> Maybe (b))))))) = 14
-- quota 1: sizeOf (Data.Maybe.fromJust :: (Maybe (((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> Maybe (b)))))))) -> (tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> Maybe (b)))))))) = 16
    
      
      return prog
  where

    -- we love partial functions
    filterBottomHack prog = do
      guard $ not $ "Data.Maybe.fromJust Data.Maybe.Nothing" `isInfixOf` show prog
      guard $ not $ "GHC.List.head []" `isInfixOf` show prog
      guard $ not $ "GHC.List.last []" `isInfixOf` show prog

    -- stream of components whose entire type unify with goal type
    inEnv = do 
            
      (id, schema) <- getUnifiedComponent :: TopDownSolver IO (Id, SType)
      
      let program = Program { content = PSymbol id, typeOf = addTrue schema }

      return program

    -- split goal into 2 goals: alpha -> T and alpha
    doSplit = do      
      let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
      let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema
      
      -- need to reset the name counter so it generates the same tau for each freshType call
      nameCtr <- getNameCounter
      alpha <- freshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') :: TopDownSolver IO RType
      setNameCounter nameCtr
      schema <- freshType (env ^. boundTypeVars) schema' :: TopDownSolver IO RType

      -- we split (?? :: T) into (??::alpha -> T) (??::alpha)
      -- subtract 1 from quota since the second term should be at least size 1
      -- TODO stop adding alphas so we don't get tau -> tau -> tau -> tau -> tau ....
      --      no components unify with (tau -> tau -> tau -> tau -> tau) and up
      --      since they all take max 3 arguments

      -- no matter what the holes are, ((?? ??) :: goalType) should be less or equal to quota
      -- guard $ (2 + sizeOfType goalType) <= sizeQuota
      -- fromJust ?? ??
      
      schemaProgram <- dfsEMode env messageChan (sizeQuota - 1) subQuota schema :: TopDownSolver IO RProgram
      let sizeQuota' = sizeQuota - sizeOf schemaProgram

      st' <- get
      let sub = st' ^. typeAssignment
      let alphaSub' = stypeSubstitute sub (shape alpha) :: SType 
      let alphaSub = addTrue alphaSub'
      
      alphaProgram <- dfsIMode env messageChan sizeQuota' subQuota alphaSub :: TopDownSolver IO RProgram

      return Program {
          content = case content schemaProgram of
                      PSymbol id -> PApp id [alphaProgram]
                      PApp id xs -> PApp id $ xs ++ [alphaProgram],
          typeOf = goalType
        }
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- moves all Data.Function functions to the end and moves the args to the front
    reorganizeSymbols :: [(Id, RSchema)]
    reorganizeSymbols = args ++ withoutDataFunctions
      where
        ogSymbols            = Map.toList $ env ^. symbols
        (args, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) ogSymbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

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
      -- liftIO $ printf "quota %d, (id, schema): %s :: %s\n\tt1: %s\n\tt2: %s\n\tinto: %s\n\tchecks: %s\n\n"
      --   quota id (show schema) (show t1) (show t2) (show $ subbedType) (show checkResult)
      
      subSize <- sizeOfSub
      guard (subSize <= subQuota)

      guard checkResult

      return (id, subbedType)
      
-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
sizeOf p = sizeOf' p -- + (sizeOfType $ typeOf p) -- TODO we need to add this back in!!!!! 

sizeOf' :: RProgram -> Int
sizeOf' p = case content p of
    PSymbol _       -> 1
    PApp _ ps       -> 1 + sum (map sizeOf' ps)
    PFun _ p1       -> 1 + sizeOf' p1
    _               -> error $ "sizeOf doesn't support this thing: " ++ (show p)


--- Data.Maybe.fromJust arg0 arg1         size 3 from sizeOf'
--- :: Maybe (a->b) -> a -> b             size 5 from sizeOfType

-- gets the size of a type (TODO are we done with this yet?)
sizeOfType :: TypeSkeleton r -> Int
sizeOfType t =
  case t of
    -- examples: 
    -- Maybe Int (size 2)
    ScalarT baseType _ -> sizeOfBase baseType
    -- examples: 
    -- tau1 -> tau2                  (size 2)
    -- tau1 -> (tau2 -> tau3)        (size 3)
    -- tau1 -> (tau2 -> Maybe b)     (size 4)
    FunctionT _ fromType toType -> sizeOfType fromType + sizeOfType toType
    _ -> error $ "sizeOfType doesn't support this thing"
  where
    sizeOfBase :: BaseType r -> Int
    sizeOfBase t' =
      case t' of
        -- examples:
        -- Int                             (size 1)
        -- Maybe Int                       (size 2)
        -- Either (Either Int Bool) Int    (size 5)
        (DatatypeT _ args _) -> 1 + (sum $ map sizeOfType args)
        -- examples: 
        -- tau0
        -- tau1
        (TypeVarT _ _) -> 1
        _ -> error $ "sizeOfBase doesn't support this thing"

-- gets the size of the sub
sizeOfSub :: TopDownSolver IO Int
sizeOfSub = do
  st <- get
  let sub = st ^. typeAssignment :: Map Id SType
  return $ Map.foldr f 0 sub
  where
    f :: SType -> Int -> Int
    f t acc = sizeOfType t + acc
