{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, syn', synMaybe, sizeOf) where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import HooglePlus.GHCChecker (check)
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
syn :: String -> IO RProgram
syn inStr = syn' inStr []

synMaybe :: IO RProgram
synMaybe = syn "Maybe (a->b) -> a -> b"

-- usage:
-- :{
-- syn' "(a -> b) -> (a -> c) -> a -> (b, c)" $ [
--   [ (["\\x -> x + 1", "\\x -> x * 3", "3"], "(4, 9)")
--   , (["\\x -> x ++ x", "Data.List.reverse", "[1,2,3]"], "([1,2,3,1,2,3], [3,2,1])")
--   ]
-- :}
syn' :: String -> [([String], String)] -> IO RProgram
syn' inStr ex = do
  env' <- readEnv $ envPath defaultSynquidParams
  env <- readBuiltinData defaultSynquidParams env'
  goal <- envToGoal env inStr
  solverChan <- newChan
  let examples = map (uncurry Example) ex
  synthesize' defaultSearchParams goal examples solverChan



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
  synthesize' searchParams goal examples messageChan
  return ()

synthesize' :: SearchParams -> Goal -> [Example] -> Chan Message -> IO RProgram
synthesize' searchParams goal examples messageChan = do
    let rawEnv = gEnvironment goal
    -- printf "rawEnv: %s\n" (show $ rawEnv ^. symbols)
    let goalType = gSpec goal :: RSchema
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    -- let hoCands = rawEnv ^. hoCandidates
    -- mapM_ print hoCands
    let symbolsWithoutFUN = Map.filterWithKey (\k a -> not $ "'ho'" `isInfixOf` k) rawSyms
    envWithHo <- do
    
    --------------------------
      -- HIGHER ORDER STUFF 
      -- let args = rawEnv ^. arguments
      -- let hoArgs = Map.filter (isFunctionType . toMonotype) args
      -- let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
      return $ rawEnv { 
          _symbols = symbolsWithoutFUN, -- `Map.union` Map.fromList hoFuns, 
          _hoCandidates = [] -- hoCands ++ map fst hoFuns
          }
    --------------------------
      -- let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      -- return $ rawEnv {
      --     _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
      --     _hoCandidates = []
      --     }
    --------------------------

    -- mapM_ print $ Map.toList $ envWithHo ^. symbols

    putStrLn "\n=================="
    putStrLn "Starting!"
    printf "Arguments: %s\n" (show $ envWithHo ^. arguments)
    let goal = shape $ lastType $ toMonotype goalType :: SType

    printf "Goal: %s\n" (show goal)
    putStrLn "=================="

    -- call dfs with iterativeDeepening
    program <- iterativeDeepening envWithHo messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    return program


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m =
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan $ (`map` [6..8]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota
  let goalType = lastType $ toMonotype goal :: RType

  -- \f g x -> (f x , g x)
  
  solution <- dfsEMode env messageChan quota goalType :: TopDownSolver IO RProgram
  
  -- check if the program has all the arguments that it should have (avoids calling check)  
  guard (filterParams solution env)
  liftIO $ printf "new program: %s\n" (show solution)
  
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
        ScalarT _ _     -> do
          liftIO $ printf "\n------\nwe are actually here scalart!!!!! \n"
          dfsEMode env messageChan quota goalType 
        FunctionT argName tArg tBody -> do
          -- add argument to new env and call dfsIMode with that new env
          liftIO $ printf "\n------\nwe are here functiont!!!!! \n"
          let newEnv = addVariable argName tArg $ addArgument argName tArg env
          liftIO $ printf "\t*** difference in env: %s\n" (show $ Map.difference (env ^. symbols) (newEnv ^. symbols))
          -- TODO what should quota be ?????? 
          body <- dfsIMode newEnv messageChan (quota - 1) tBody
          -- return \argName -> (dfs on tBody)
          let program = Program { content = PFun argName body, typeOf = goalType }
          guard (sizeOf program <= quota)
          return program
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
      
      
      -- when (quota >= 5 && fst component == "Pair") $ liftIO $ printf "pair unifies with %s in e-mode\n" (show goalType)
      
      (id, schema) <- getUnifiedComponent :: TopDownSolver IO (Id, SType)
      let program = Program { content = PSymbol id, typeOf = refineTop env schema }
      liftIO $ printf "found    (%s) --- %s \n" (show goalType) (show program)

      -- when (quota >= 5 && fst component == "Pair") $ liftIO $ printf "  and gets this program: %s\n" (show program)

      guard (sizeOf program <= quota)
      return program

    -- split goal into 2 goals: alpha -> T and alpha
    doSplit = do      
      -- TODO need to freshType alpha so it turns into the same tau2 as schema
      let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
      let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema
      
      -- reset the name counter so it generates the same tau
      indices <- getNameCounter :: TopDownSolver IO (Map Id Int)
      alpha <- freshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') :: TopDownSolver IO RType
      setNameCounter indices
      
      schema <- freshType (env ^. boundTypeVars) schema' :: TopDownSolver IO RType

      -- (?? :: T)     quota 5
      -- (??::alpha -> T  quota 5) (??::alpha    quota 5-n)


      liftIO $ printf "splitting (%s) up into: (%s) and (%s)\n" (show goalType) (show schema) (show alpha)
      -- first split: 
          {-

                alpha -> (b,c)
                beta -> alpha -> (b,c)
          -}



      schemaProgram <- dfsEMode env messageChan (quota - 1) schema :: TopDownSolver IO RProgram
      let quota' = quota - sizeOf schemaProgram
      st' <- get
      let sub = st' ^. typeAssignment
      let alphaSub = stypeSubstitute sub (shape alpha)
      liftIO $ printf "refined alpha into alphaSub = %s\n" (show alphaSub)
      
-- running dfs on <d> . <c> . <b> . <a> . (((((b -> (a -> c))) -> d)) -> (((a -> (b -> c))) -> d)) at size 7
-- splitting (d) up into: ((tau0 -> d)) and (tau0)
-- found    ((tau0 -> d)) --- arg0 
-- refined alpha into alphaSub = b -> a -> c
-- i-mode
-- a -> c
-- c
-- splitting (c) up into: ((tau1 -> c)) and (tau1)
-- splitting ((tau1 -> c)) up into: ((tau2 -> (tau1 -> c))) and (tau2)
-- found    ((tau2 -> (tau1 -> c))) --- arg1 

      alphaProgram <- dfsIMode env messageChan (quota') (refineTop env alphaSub) :: TopDownSolver IO RProgram

      -- TODO is this the way we want to be building up our programs? the parens are weird but technically still work 
      return Program {
          -- content = PApp "(Data.Function.$)" [schemaProgram, alphaProgram],
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
        ogSymbols                             = Map.toList $ env ^. symbols
        (dollar, withoutDollar)               = partition (("$" `isInfixOf`) . fst) ogSymbols
        (_, withoutAnd)                       = partition (("&" `isInfixOf`) . fst) withoutDollar
        (args, withoutArgs)                   = partition (("arg" `isInfixOf`) . fst) withoutAnd
        (dataFunctions, withoutDataFunctions) = partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns a reified version of schema (no type vars) w/ sub if unifies 
    getUnifiedComponent :: TopDownSolver IO (Id, SType)
    getUnifiedComponent  = do

      (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)
      -- helper fn for guards
      let guardExclude = guard . not . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
      let guardInclude = guard . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
      -- -- guardExclude ["List", "fix", "Data.Tuple"]
      -- guardInclude ["$", "arg", "fromJust"]
      guardInclude ["arg"]

      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape freshVars :: SType
      let t2 = shape goalType :: SType
      when (quota >= 3 && id == "Pair") $ liftIO $ printf "we're looking at pair for goal %s in e-mode! (t1, t2) = (%s, %s)\n" (show goalType) (show t1) (show t2)
      -- syn "arg0:((b->a->c)->d)->arg1:(a->b->c)->d"       solution: arg0 (\arg2 arg3 -> arg1 arg3 arg2)
      

      modify $ set isChecked True
      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      
      st' <- get

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      -- liftIO $ printf "unifying goal: (%s) with (%s, %s) ===> %s \n" (show goalType) id (show schema) (show checkResult)
      -- unifying t1 (tau3) and t2 ((tau2 , tau1) -> Int) ===> isChecked: True
      -- let t1S = 
      -- when (isFunctionType t1 && isFunctionType t2 ) $ liftIO $ printf "unifying t1 (%s) and t2 (%s) ===> isChecked: %s\n" (show t1) (show t2) (show checkResult)

      guard checkResult
      when (quota >= 3 && id == "Pair") $ liftIO $ printf "    we got past the guard!!!\n"

      return (id, stypeSubstitute sub (shape freshVars))

-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
sizeOf p = sizeOf' p -- + (sizeOfType $ typeOf p) TODO we need to add this back in!!!!! 
  where
    -- doesn't consider the size of the type
    sizeOf' :: RProgram -> Int
    sizeOf' p = case content p of
        PSymbol _       -> 1
        PApp _ ps       -> 1 + sum (map sizeOf' ps)
        PFun _ p1       -> 1 + sizeOf' p1
        _               -> error $ "sizeOf doesn't support this thing: " ++ (show p)

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

