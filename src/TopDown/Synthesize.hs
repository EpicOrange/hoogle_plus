{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, syn', sizeOf) where

-- import HooglePlus.TypeChecker
import TopDown.TypeChecker
import HooglePlus.GHCChecker (check)
import Synquid.Error
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
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

    -- \xs x -> Data.List.foldr ($) x xs
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
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan $ (`map` [6]) $ \quota -> do
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota

  let goalType = shape $ lastType $ toMonotype goal :: SType
  solution <- dfs env messageChan quota goalType :: TopDownSolver IO RProgram
  
  -- check if the program has all the arguments that it should have (avoids calling check)
  -- liftIO $ printf "size %d/%d solution: %s\n" (sizeOf solution) quota (show solution)
  
--   liftIO $ printf "found... %s \n" (show solution)

  guard (filterParams solution env)
  
  -- call check on the program
  -- liftIO $ printf "size %d/%d solution: %s\n" (sizeOf solution) quota (show solution)
  -- liftIO $ printf "checking... %s\n" (show solution)
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
-- does DFS stuff, with max size of program given as a quota
--
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownSolver IO RProgram
dfs env messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = do
    -- stream of components that unify with goal type (which we might use to fill the holes)
    component <- choices $ Map.toList (env ^. symbols)
    -- when (quota == 6) $ guard ("$" `isInfixOf` fst component)
    -- when (quota >= 4) $ liftIO $ printf "quota %d, at component: %s\n" quota (show component)
    -- when (quota <= 5) $ guard (fst component == "Data.Maybe.fromJust" || "arg" `isPrefixOf` fst component)
    
    -- when (quota >= 4) $ liftIO $ printf "!!!! quota %d, goal %s, component is %s,\n" quota (show goalType) (show component)
    (id, schema) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
--     when (quota >= 4) $ liftIO $ printf "!!!! quota %d, goal %s, component is %s, unified to %s\n" quota (show goalType) (show component) (show schema)

    -- stream of solutions to the (id, schema) returned from getUnifiedComponent
    if isGround schema
      then return Program { content = PSymbol id, typeOf = refineTop env schema }
      else do

  --       liftIO $ printf "goal: %s\tsplitting up args for function %s: %s\n" (show goalType) id (show schema)
        -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]
        -- let args = namedArgTypes schema :: [(Id, SType)]

        let fillArg :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
            fillArg (quota', programs) arg@(ScalarT _ _) = do
              program <- dfs env messageChan quota' arg
              return (quota' - sizeOf program, programs ++ [program])
            fillArg (quota', programs) arg = do -- arg is a function type, e.g. a -> Bool
              let newArgs = namedArgTypes arg :: [(Id, SType)]
              let (newEnv, _) = updateEnvWithSpecArgs (refineTop env arg) env :: (Environment, RType)

              body <- dfs newEnv messageChan quota' arg -- call dfs with this function as a goal
              let program = mkLambda newArgs body

              let quota'' = quota' - sizeOf program
              guard (quota'' >= 0)
              return (quota'', programs ++ [program])

        (_, argsFilled) <- foldM fillArg (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])
        return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 

  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- turn a function type to a list of its arguments
    namedArgTypes :: SType -> [(Id, SType)]
    namedArgTypes (FunctionT id t body) = (id, t) : namedArgTypes body
    namedArgTypes _ = []
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- build a lambda function given args and body
    mkLambda :: [(Id, SType)] -> RProgram -> RProgram
    mkLambda args body = foldr addArg body args
      where
        -- add the given argument to body
        addArg :: (Id, SType) -> RProgram -> RProgram
        addArg (id',t') body''@(Program _ bodyType) =
          Program { 
            content = PFun id' body'', 
            typeOf = FunctionT id' (refineTop env t') bodyType
          }

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns a reified version of schema (no type vars) w/ sub if unifies 
    getUnifiedComponent :: (Id, RSchema) -> TopDownSolver IO (Id, SType)
    getUnifiedComponent (id, schema) = do
      -- helper fn for guards
      let guardAll = guard . all (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
      guardAll ["foldr", "$", "arg"]

      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      st' <- get

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      
      guard checkResult
      -- when ("fromJust" `isInfixOf` id) $
        -- liftIO $ printf "-------\noverall goal: %s\t component: %s (freshvars: %s) \t====> %s\n-------\n" (show t2) (show t1) (show freshVars) (show $ stypeSubstitute sub (shape freshVars))

      return (id, stypeSubstitute sub (shape freshVars))

-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
-- sizeOf = length . words . show
sizeOf p =
  case content p of
    PSymbol _       -> 1
    PApp _ ps       -> 1 + sum (map sizeOf ps)
    PFun _ p1       -> 1 + sizeOf p1
    _               -> error $ "sizeOf doesn't support this thing: " ++ (show p)
    -- the rest aren't being synthesized
--     PIf p1 p2 p3    -> sizeOf p1 + sizeOf p2 + sizeOf p3
--     PMatch p1 cases -> sizeOf p1 + sum (map (sizeOf . expr) cases)
--     PFix _ p1       -> sizeOf p1
--     PLet _ p1 p2    -> sizeOf p1 + sizeOf p2
--     _               -> 0

