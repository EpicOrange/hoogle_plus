{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.SynthesizeOptimizedOld() where

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
  let examples = map (uncurry Example) $ [(["Nothing", "1"], "Right 1"), (["Just 2", "3"], "Left 2")]
  synthesize defaultSearchParams goal examples solverChan



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
    let symbolBuckets = reorganizeSymbols' $ Map.toList symbolsWithoutFUN
    print $ Map.map length symbolBuckets

    -- fromList [(0,28),(1,65),(2,111),(3,132),(4,134)]
    -- fromList [(0,28),(1,37),(2,46),(3,21),(4,2)]
    
    let env = rawEnv { _symbols = symbolsWithoutFUN, _hoCandidates = [] }

    -- putStrLn "\n=================="
    -- putStrLn "Starting!"
    printf "Arguments: %s\n" (show $ env ^. arguments)
    -- let goal = shape $ lastType $ toMonotype goalType :: SType

    -- printf "Goal: %s\n" (show goal)
    -- mapM_ print (Map.keys $ envWithHo ^. symbols)
    -- putStrLn "=================="

    -- call dfs with iterativeDeepening
    program <- iterativeDeepening env symbolBuckets messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    -- return program
    return ()
  where
    -- moves all Data.Function functions to the end and moves the args to the front
    reorganizeSymbols :: [(Id, RSchema)] -> [(Id, RSchema)]
    reorganizeSymbols symbols = args ++ withoutDataFunctions
      where
        (args, withoutArgs)  = partition (("arg" `isInfixOf`) . fst) symbols
        withoutDataFunctions = snd $ partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- make component map
    -- TODO we do this on every call to dfs, which is unnecessary
    --    the only things that change are the added arguments when doing lambdas
    reorganizeSymbols' :: [(Id, RSchema)] -> Map Int [(Id, RSchema)]
    reorganizeSymbols' = mergeUp . bucketBy (countArrows . toMonotype . snd) . reorganizeSymbols
      where
        bucketBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
        bucketBy f xs = Map.fromListWith (++) [(f x, [x]) | x <- xs]
        mergeUp :: (Ord k) => Map k [a] -> Map k [a]
        mergeUp = snd . Map.mapAccum (\acc xs -> (acc++xs, acc++xs)) []




-- type TopDownSolver m = StateT CheckerState (LogicT m)

-- evalTopDownSolver :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
-- evalTopDownSolver messageChan m =
--   observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m


type MemoMap = Map (RType, Int, Map Id SType) (Logic RProgram) -- (query, quota, sub) ==> program
type TopDownSolver m = StateT CheckerState (LogicT (StateT MemoMap m))

evalTopDownSolver :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolver messageChan m =
  (`evalStateT` Map.empty) $ observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- convert to Logic a
choices :: (Traversable f, MonadPlus m) => f a -> m a
choices = msum . fmap return

memoizeProgram :: Int -> RType -> TopDownSolver IO RProgram -> TopDownSolver IO RProgram
memoizeProgram _ _ compute = compute
-- memoizeProgram quota goalType compute = do
  -- st <- get
  -- let sub = st ^. typeAssignment
  -- let key = (goalType, quota, sub)
  -- memoMap <- lift $ lift $ get :: TopDownSolver IO MemoMap
  -- -- TODO maybe use Map.lookup with default compute??????
  -- case Map.lookup key memoMap of
  --   Just progs -> do
  --     prog <- choices progs 
  --     -- when ("fst" `isInfixOf` show prog || "snd" `isInfixOf` show prog) $ liftIO $ printf "quota (%d): we found one! goal (%s), prog (%s)\n" quota (show goalType) (show prog)
  --     -- liftIO $ printf "key and program retrieved:\n\t%s\n\t%s\n" (show key) (show prog)
  --     return prog
  --   Nothing    -> do
  --     prog <- compute
  --     -- liftIO $ printf "key and program added:\n\t%s\n\t%s\n" (show key) (show prog)
  --     lift $ lift $ modify (Map.insertWith mplus key (return prog))
  --     return prog

-- synGuardO "Maybe a -> b -> Either a b" ["maybe", ".Right", ".Left"]
-- \arg0 arg1 -> Data.Maybe.maybe (Data.Either.Right arg1) Data.Either.Left arg0
-- Data.Either.Right (Data.Maybe.maybe arg1 (\\arg2 -> arg1) arg0)

{-
[Int] -> Int
tau0  -> Int

tau0 ==> [Int]


sub [tau6 ==> Int]


 comes from f (tau6 means something)
tau6   (different than previous one)

-}



-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Map Int [(Id, RSchema)] -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env buckets messageChan searchParams examples goal = evalTopDownSolver messageChan $ (`map` [1..]) $ \quota -> do
  
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota
  let goalType = lastType $ toMonotype goal :: RType

  solution <- dfsEMode env buckets messageChan quota goalType :: TopDownSolver IO RProgram
  
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

-- determines if the result has all the appropriate arguments ()
filterParams :: RProgram -> Environment -> Bool
filterParams program env = all (`isInfixOf` show program) $ filter (not . ("tcarg" `isInfixOf`) ) $ Map.keys $ env ^. arguments

--
-- does DFS in I-mode
--    * if is a function type, split args out and return a lambda 
--    * if not, searched in e-mode 
--
dfsIMode :: Environment -> Map Int [(Id, RSchema)] -> Chan Message -> Int -> RType -> TopDownSolver IO RProgram
dfsIMode env buckets messageChan quota goalType 
  | quota <= 0 = mzero
  | otherwise = memoizeProgram quota goalType $
      case goalType of
        
        -- if function type, split up the arguments and add them to the environment
        FunctionT _ tArg tBody -> do
          argName <- freshId (Map.keys $ env ^. arguments) "arg"

          -- add argument to new env and call dfsIMode with that new env
          let env' = addVariable argName tArg $ addArgument argName tArg env
          -- "arg0", a -> b -> c
          let numArrows = countArrows tArg
          let newArg = (argName, Monotype tArg) :: (Id, RSchema)
          let buckets' = Map.mapWithKey (\i bucket -> if i >= numArrows then newArg : bucket else bucket) buckets
          
          -- we're synthesizing the body for the lambda
          -- so we subtract 1 from the body's quota to account for the lambda we'll be returning
          body <- dfsIMode env' buckets messageChan (quota - 1) tBody

          let program = Program { content = PFun argName body, typeOf = goalType }
          guard (sizeOf program <= quota)
          return program
        
        -- not a function type, switch into e-mode
        ScalarT _ _     -> do
          dfsEMode env buckets messageChan quota goalType 
        
        _ -> error "unsupported goalType for dfsIMode"

--
-- does DFS in E-mode
--    * checks if anything in the environment matches the full type of goal
--    * if not, splits it up into 2 new goals: alpha -> T and alpha 
--
dfsEMode :: Environment -> Map Int [(Id, RSchema)] -> Chan Message -> Int -> RType -> TopDownSolver IO RProgram
dfsEMode env buckets messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = memoizeProgram quota goalType $ do
      prog <- inEnv `mplus` doSplit
      -- we love partial functions
      guard $ not $ "Data.Maybe.fromJust Data.Maybe.Nothing" `isInfixOf` show prog
      guard $ not $ "GHC.List.head []" `isInfixOf` show prog
      guard $ not $ "GHC.List.last []" `isInfixOf` show prog
      return prog
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
      schemaProgram <- dfsEMode env buckets messageChan (quota - 1) schema :: TopDownSolver IO RProgram
      let quota' = quota - sizeOf schemaProgram
      
      st' <- get
      let sub = st' ^. typeAssignment
      let alphaSub' = stypeSubstitute sub (shape alpha) :: SType 
      let alphaSub = addTrue alphaSub'
      
      alphaProgram <- dfsIMode env buckets messageChan quota' alphaSub :: TopDownSolver IO RProgram

      return Program {
          content = case content schemaProgram of
                      PSymbol id -> PApp id [alphaProgram]
                      PApp id xs -> PApp id $ xs ++ [alphaProgram],
          typeOf = goalType
        }
    
    -- Using the components in env, like ("length", <a>. [a] -> Int)
    -- tries to instantiate each, replacing type vars in order to unify with goalType
    getUnifiedComponent :: TopDownSolver IO (Id, SType)
    getUnifiedComponent = do

      -- (id, schema) <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)
      (id, schema) <- choices $ Map.findWithDefault [] (countArrows goalType) buckets :: TopDownSolver IO (Id, RSchema)
      -- (id, schema) <- choices $ maybe [] snd $ Map.lookupMax buckets :: TopDownSolver IO (Id, RSchema)

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
      
      guard checkResult

      return (id, subbedType)

countArrows :: RType -> Int
countArrows (FunctionT _ tArg tBody) = 1 + countArrows tBody
countArrows _                        = 0

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

