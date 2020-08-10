{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, syn', sizeOf) where

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
    -- printf "Arguments: %s\n" (show $ envWithHo ^. arguments)
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
  
  -- liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota
  let goalType = lastType $ toMonotype goal :: RType

  solution <- dfsEMode env messageChan quota goalType :: TopDownSolver IO RProgram
  
  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution env)
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

-- determines if the result has all the appropriate arguments
filterParams :: RProgram -> Environment -> Bool
filterParams program env = all (`isInfixOf` show program) $ Map.keys $ env ^. arguments

--
-- does DFS in I-mode
--    * if is a function type, split args out and return a lambda 
--    * if not, searched in e-mode 
--
dfsIMode :: Environment -> Chan Message -> Int -> RType -> TopDownSolver IO RProgram
dfsIMode env messageChan quota goalType 
  | quota <= 0 = mzero
  | otherwise =
      case goalType of
        
        -- if function type, split up the arguments and add them to the environment
        FunctionT _ tArg tBody -> do
          argName <- freshId (Map.keys $ env ^. arguments) "arg"

          -- add argument to new env and call dfsIMode with that new env
          let newEnv = addVariable argName tArg $ addArgument argName tArg env

          -- we're synthesizing the body for the lambda
          -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
          body <- dfsIMode newEnv messageChan (quota - 1) tBody

          let program = Program { content = PFun argName body, typeOf = goalType }
          guard (sizeOf program <= quota)
          return program
        
        -- not a function type, switch into e-mode
        ScalarT _ _     -> do
          dfsEMode env messageChan quota goalType 
        
        _ -> error "unsupported goalType for dfsIMode"

--
-- does DFS in E-mode
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
--
dfsEMode :: Environment -> Chan Message -> Int -> RType -> TopDownSolver IO RProgram
dfsEMode env messageChan quota goalType 
  | quota <= 0 = mzero
  | otherwise  = inEnv `mplus` doSplit
    
  where

    -- stream of components whose entire type unify with goal type
    inEnv = do 
            
      (id, schema) <- getUnifiedComponent :: TopDownSolver IO (Id, SType)
      let program = Program { content = PSymbol id, typeOf = addTrue schema }

      guard (sizeOf program <= quota)
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
      schemaProgram <- dfsEMode env messageChan (quota - 1) schema :: TopDownSolver IO RProgram
      let quota' = quota - sizeOf schemaProgram
      
      st' <- get
      let sub = st' ^. typeAssignment
      let alphaSub' = stypeSubstitute sub (shape alpha) :: SType 
      let alphaSub = addTrue alphaSub'
      
      alphaProgram <- dfsIMode env messageChan (quota') alphaSub :: TopDownSolver IO RProgram

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
    reorganizeSymbols = args ++ withoutDataFunctions -- ++ dataFunctions -- ++ dollar
      where
        ogSymbols            = Map.toList $ env ^. symbols
        (args, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) ogSymbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns a reified version of schema (no type vars) w/ sub if unifies 
    getUnifiedComponent :: TopDownSolver IO (Id, SType)
    getUnifiedComponent  = do

      (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)

      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType

      modify $ set isChecked True
      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      st' <- get

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked

      guard checkResult

      return (id, stypeSubstitute sub (shape freshVars))
      
      where 
        -- usage:
        -- guardInclude id ["Pair", "arg"]
        guardExclude id = guard . not . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
        guardInclude id = guard . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()


-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
sizeOf p = sizeOf' p -- + (sizeOfType $ typeOf p)  TODO we need to add this back in!!!!! 
  where
    -- doesn't consider the size of the type
    sizeOf' :: RProgram -> Int
    sizeOf' p = case content p of
        PSymbol _       -> 1
        PApp _ ps       -> 1 + sum (map sizeOf' ps)
        PFun _ p1       -> 1 + sizeOf' p1
        _               -> error $ "sizeOf doesn't support this thing: " ++ (show p)

-- gets the size of a type (TODO are we done with this yet?)
sizeOfType :: TypeSkeleton r -> Int
sizeOfType t =
  case t of
    ScalarT baseType _ -> sizeOfBase baseType
    FunctionT _ fromType toType -> sizeOfType fromType + sizeOfType toType
    _ -> error $ "sizeOfType doesn't support this thing"
  where
    sizeOfBase :: BaseType r -> Int
    sizeOfBase t' =
      case t' of
        (DatatypeT _ args _) -> 1 + (sum $ map sizeOfType args)
        (TypeVarT _ _) -> 1
        _ -> error $ "sizeOfBase doesn't support this thing"

