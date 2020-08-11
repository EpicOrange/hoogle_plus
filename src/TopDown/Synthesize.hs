{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, synGuard, syn', sizeOf) where

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
    printf "Arguments: %s\n" (show $ env ^. arguments)
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
  | otherwise  = do
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
      schemaProgram <- dfsEMode env messageChan (quota - 1) schema :: TopDownSolver IO RProgram
      let quota' = quota - sizeOf schemaProgram
      
      st' <- get
      let sub = st' ^. typeAssignment
      let alphaSub' = stypeSubstitute sub (shape alpha) :: SType 
      let alphaSub = addTrue alphaSub'
      
      alphaProgram <- dfsIMode env messageChan quota' alphaSub :: TopDownSolver IO RProgram

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
      -- solveTypeConstraint' t1 t2 :: TopDownSolver IO ()
      -- lookup
      -- syn "Eq a => [(a,b)] -> a -> b"
      -- \xs k -> Data.Maybe.fromJust (Data.List.lookup k xs)
      
      st' <- get

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked

      let subbedType = stypeSubstitute sub (shape freshVars)
      -- liftIO $ printf "quota %d, (id, schema): %s :: %s\n\tt1: %s\n\tt2: %s\n\tinto: %s\n\n"
      --   quota id (show schema) (show t1) (show t2) (show $ subbedType)
      
      guard checkResult

      return (id, subbedType)
      
      where 
        -- usage:
        -- guardInclude id ["Pair", "arg"]
        -- guardExclude id = guard . not . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
        -- guardInclude id = guard . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()

        solveTypeConstraint' :: SType -> SType -> TopDownSolver IO ()
        solveTypeConstraint' (FunctionT id tArg tBody) t2
          | "@@hplusTC@@" `isInfixOf` (show tArg) = do
                -- take off the first arg
                solveTypeConstraint' tBody t2
                st' <- get
                let sub = st' ^. typeAssignment
                let subbedArg = stypeSubstitute sub tArg
                let tcSymbols = filter ("@@hplusTC@@" `isInfixOf`) $ map (show . snd) reorganizeSymbols
                liftIO $ printf "subbedArg: %s, symbols: %s \n" (show subbedArg) (show tcSymbols)
                guard (show subbedArg `elem` tcSymbols)

                -- in our tcSymbols: <a> . (@@hplusTC@@Eq (a) -> a)
                -- @@hplusTC@@Eq (tau3)
                -- tau5
                
        solveTypeConstraint' t1 t2 = solveTypeConstraint env t1 t2

{-

hi zheng. we're running into an issue where we have to unify a component with a typeclass.
like `lookup :: <a> . Eq a => a -> [(a, b)] -> Maybe b`
but `Eq a` doesn't unify with `tau3` because in `solveTypeConstraint`, a type class cannot unify with a type variable, based on the following code: 

```
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) (ScalarT (DatatypeT dt _ _) _)
  | tyclassPrefix `isPrefixOf` dt = modify $ set isChecked False -- type class cannot unify with a type variable
```

we commented out this part of the code, and now the query that was previously without solution has a solution. We tried to think about ways this might break the code, and so far we haven't come up with any. Thoughts?


query: "Eq a => [(a,b)] -> a -> b"
  tcarg0   :: @@hplusTC@@Eq (a)
  arg0     :: [(a,b)]
  arg1     :: a
  fromJust :: <tau0> . (Maybe (tau0) -> tau0)
  lookup   :: <tau1> . <tau2> . @@hplusTC@@Eq (tau2) -> tau2 -> [(tau2, tau1)] -> Maybe tau1

solution    ?? :: b [e-mode]
solution    (?? :: alpha -> b [e-mode]) (?? :: alpha [i-mode])
                ==> unify fromJust, 
                alpha ~ Maybe b

solution    fromJust (?? :: Maybe b [i-mode])
                ==> split

solution    fromJust ((?? :: beta -> Maybe b [e-mode]) (?? :: beta [i-mode]))
                ==> split

solution    fromJust ((?? :: gamma -> beta -> Maybe b [e-mode]) (?? :: gamma [i-mode]) (?? :: beta [i-mode])
                ==> split

solution    fromJust ((?? :: delta -> gamma -> beta -> Maybe b [e-mode]) (?? :: delta [i-mode]) (?? :: gamma [i-mode]) (?? :: beta [i-mode])
                ==> unify lookup :: @@hplusTC@@Eq (tau3) -> tau3 -> [(tau3, tau2)] -> Maybe tau2
                delta ~ @@hplusTC@@Eq (tau3)
                gamma ~ tau3
                tau2  ~ b
                beta  ~ [(tau3, tau2)] ~ [(tau3, b)]

solution    fromJust (lookup (?? :: @@hplusTC@@Eq (tau3) [i-mode]) (?? :: tau3 [i-mode]) (?? :: [(tau3, b)] [i-mode])
                ==> unify with tcarg0 :: @@hplusTC@@Eq (a)
                tau3 ~ a

solution    fromJust (lookup (tcarg0) (?? :: a [i-mode]) (?? :: [(a, b)] [i-mode])
                ==> unify arg0 and arg1

solution    fromJust (lookup tcarg0 arg0 arg1)




Eq a => a -> [a] -> Maybe a	
\arg0 arg1 -> bool Nothing (Just arg0) (GHC.List.elem arg0 arg1)
-}

-- subbedArg: @@hplusTC@@Eq (tau3), symbols: ["<a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))"
-- "<a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))"
-- "@@hplusTC@@Eq (Bool)"
-- "@@hplusTC@@Eq (Char)"
-- "@@hplusTC@@Eq (Double)"
-- "@@hplusTC@@Eq (Float)"
-- "@@hplusTC@@Eq (Int)"
-- "@@hplusTC@@Eq (Unit)"
-- "@@hplusTC@@Num (Double)"
-- "@@hplusTC@@Num (Float)"
-- "@@hplusTC@@Num (Int)"
-- "@@hplusTC@@Ord (Bool)"
-- "@@hplusTC@@Ord (Char)"
-- "@@hplusTC@@Ord (Double)"
-- "@@hplusTC@@Ord (Float)"
-- "@@hplusTC@@Ord (Int)"
-- "<b> . <a> . (@@hplusTC@@Show (a) -> (@@hplusTC@@Show (b) -> @@hplusTC@@Show ((Either (a) (b)))))"
-- "@@hplusTC@@Show (Bool)"
-- "@@hplusTC@@Show (Char)"
-- "@@hplusTC@@Show (Double)"
-- "@@hplusTC@@Show (Float)"
-- "@@hplusTC@@Show (Int)"
-- "@@hplusTC@@Show (Unit)"
-- "<b> . <a> . (@@hplusTC@@Read (a) -> (@@hplusTC@@Read (b) -> @@hplusTC@@Read ((Either (a) (b)))))"
-- "<b> . <a> . (@@hplusTC@@Ord (a) -> (@@hplusTC@@Ord (b) -> @@hplusTC@@Ord ((Either (a) (b)))))"
-- "<b> . <a> . (@@hplusTC@@Eq (a) -> (@@hplusTC@@Eq (b) -> @@hplusTC@@Eq ((Either (a) (b)))))"
-- "<b> . <a> . @@hplusTC@@Semigroup ((Either (a) (b)))"
-- "<a> . (@@hplusTC@@Eq (a) -> @@hplusTC@@Eq (([a])))"
-- "<a> . (@@hplusTC@@Eq (a) -> ([a] -> [[a]]))"
-- "<a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))"
-- "<b> . <a> . (@@hplusTC@@Eq (a) -> (a -> ([(a , b)] -> Maybe (b))))"
-- "<a> . (@@hplusTC@@Ord (a) -> ([a] -> a))"
-- "<a> . (@@hplusTC@@Ord (a) -> ([a] -> a))"
-- "<a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))"
-- "<a> . (@@hplusTC@@Num (a) -> ([a] -> a))"
-- "<a> . (@@hplusTC@@Num (a) -> ([a] -> a))"
-- "<a> . (@@hplusTC@@Show (a) -> (a -> [Char]))"
-- "<a> . (@@hplusTC@@Show (a) -> ([a] -> ([Char] -> [Char])))"
-- "<a> . (@@hplusTC@@Show (a) -> (a -> ([Char] -> [Char])))"
-- "<a> . (@@hplusTC@@Show (a) -> (Int -> (a -> ([Char] -> [Char]))))"] 
-- quota 2, (id, schema): GHC.List.lookup :: <b> . <a> . (@@hplusTC@@Eq (a) -> (a -> ([(a , b)] -> Maybe (b))))
--         t1: @@hplusTC@@Eq (tau4) -> tau4 -> [(tau4 , tau3)] -> Maybe (tau3)
--    tau4 -> [(tau4 , tau3)] -> Maybe (tau3)
--    Int  -> [(Int , Bool)] -> Maybe (Bool)
--    guard "@@hplusTC@@Eq (Int) is in env"
--         t2: tau2                 -> tau1 -> tau0            -> Maybe (b)
--         in: @@hplusTC@@Eq (tau1) -> tau1 -> [(tau1 , b)]    -> Maybe (b)
--         checkResult: False



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

