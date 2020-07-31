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
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan $ (`map` [4]) $ \quota -> do
  
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
    
    -- liftIO $ print "--------------"
    -- liftIO $ mapM_ print $ reorganizeSymbols
    -- liftIO $ print "--------------"

    -- stream of components that unify with goal type (which we might use to fill the holes)
    component <- choices $ reorganizeSymbols :: TopDownSolver IO (Id, RSchema)
    when (fst component == "arg1" && "a" == (show goalType)) $ liftIO $ printf "comparing (a) to %s\n" (show component)
    -- when (quota == 6) $ guard ("$" `isInfixOf` fst component)
    -- when (quota >= 4) $ liftIO $ printf "quota %d, at component: %s\n" quota (show component)
    -- when (quota <= 5) $ guard (fst component == "Data.Maybe.fromJust" || "arg" `isPrefixOf` fst component)
    
-- arg1 in env is of type Just a
-- before guard Data.Maybe.fromJust arg0
-- unifying $, rest :: tau1 -> b, goal :: b, result = False
-- fillArgs rest (tau1 -> b) quota' (1)
-- doing dfs on: a
--         with sub fromList [("tau0",b),("tau1",a),("tau2",a -> b)]
-- arg1 in env is of type Just a
-- comparing (a) to ("arg1",a)            <-- TODO WHAT
-- doing dfs on: tau5 -> Maybe (Maybe (tau1 -> b))

    -- when (quota >= 4) $ liftIO $ printf "!!!! quota %d, goal %s, component is %s,\n" quota (show goalType)
    (id, schema) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
    when (id == "arg1" && "a" == (show goalType)) $ liftIO $ printf "(a) unified with %s\n" (show id)
    -- when (quota >= 3) $ liftIO $ printf "!!!! quota %d, goal %s\n\tcomponent is %s\n\tunified to %s\n" quota (show goalType) (show component) (show schema)
    -- liftIO $ printf "goal %s, component is %s, unified to %s\n" (show goalType) (show component) (show schema)
-- \arg0 -> (:) arg0 []
-- !!!! quota 3, goal [[a]]
--         component is ("Cons",<a> . (a -> ([a] -> {[a]|_v == (Cons x xs)})))
--         unified to [a] -> [[a]] -> [[a]]
-- !!!! quota 4, goal tau5 -> tau3 -> tau1 -> b, component is ("(Data.Function.$)",<b> . <a> . (((a -> b)) -> (a -> b))),
-- unified to (tau7 -> tau5 -> tau3 -> tau1 -> b) -> tau7 -> tau5 -> tau3 -> tau1 -> b

    -- stream of solutions to the (id, schema) returned from getUnifiedComponent
    if isGround schema
      -- this is a ground type, no args need to be filled, we're done
      then return Program { content = PSymbol id, typeOf = refineTop env schema }
      -- this is a function type
      else do

            {-
            args = [Maybe (tau1 -> b), tau1]

            1. find stuff for Maybe (tau1 -> b)

                      find arg0

            2. check if remaining type unifies with OG goalType tau1 -> b 

                  => it does, so return here instead of going to next argument

            Idea so I don’t forget: do the thing we’re talking about now first, and then do lambdas if we can’t find anything

            -}

        -- when ("$" `isInfixOf` id) $ liftIO $ printf "full schema: %s\n" (show schema)
        -- recurse, returning filled arguments
        -- may return without filling all the args if the goal type is an arrow type
        let fillArgs :: SType -> Int -> TopDownSolver IO [RProgram]
            fillArgs schemaStype quota = 
              case getFirstArg schemaStype of
                Nothing          -> return []
                Just (arg, rest) -> do

-- full schema: (tau1 -> b) -> tau1 -> b
-- before guard Data.Maybe.fromJust arg0
--   tau1 ==> a
-- unifying $, rest :: tau1 -> b, goal :: b, result = False
-- program <- dfs ... goal is "tau1"
                  st'' <- get
                  let sub = st'' ^. typeAssignment

                  let arg' = stypeSubstitute sub arg
                  when ("$" `isInfixOf` id) $ liftIO $ printf "doing dfs on: %s\n\twith sub %s\n" (show arg') (show sub)
                  when ("$" `isInfixOf` id) $ liftIO $ printf "arg1 in env is of type %s\n" (show $ Map.lookup "arg1" $ env ^. symbols)
-- doing dfs on: tau1
--         with sub fromList [("tau0",b),("tau1",a),("tau2",a -> b)]
                
                  program <- dfs env messageChan quota arg'
                  let quota' = quota - sizeOf program
                  when ("$" `isInfixOf` id) $ liftIO $ printf "before guard %s\n" (show program)
                  guard (quota' >= 0)
                  -- when ("$" `isInfixOf` id) $ liftIO $ printf "here2 %s\n" (show program)

                  -- check if we can return without filling all the args
                  -- (the rest of the type matches the goal type)
                  -- if goal is tau1 -> b
                  -- and we fill args and find that the rest of the type is tau1 -> b
                  -- then return this as a partial application
                  solveTypeConstraint env rest goalType :: TopDownSolver IO ()
                  -- solveTypeConstraint env goalType rest :: TopDownSolver IO ()
                  st' <- get

                  let checkResult = st' ^. isChecked
                  -- when ("fromJust" `isInfixOf` id) $ liftIO $ printf "unifying fromJust, rest :: %s, goal :: %s, result = %s\n" (show rest) (show goalType) (show checkResult)
                  when ("$" `isInfixOf` id) $ liftIO $ printf "unifying $, rest :: %s, goal :: %s, result = %s\n" (show rest) (show goalType) (show checkResult)
                  
                  if checkResult 
                    -- if the rest unifies with goalType
                    then return [program]

                    -- if we need to unify further
                    else do
                      when ("$" `isInfixOf` id) $ liftIO $ printf "fillArgs rest (%s) quota' (%s)\n" (show rest) (show quota')
                      programs <- fillArgs rest quota'
                      when ("$" `isInfixOf` id) $ liftIO $ printf "afterwards %s \n" (show programs)
                      return (program : programs)
        {-
       
running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 4
full schema: (tau4 -> Maybe (Maybe (Maybe (b)))) -> tau4 -> Maybe (Maybe (Maybe (b)))
full schema: (tau3 -> Maybe (Maybe (b))) -> tau3 -> Maybe (Maybe (b))
full schema: (tau5 -> tau3 -> Maybe (Maybe (b))) -> tau5 -> tau3 -> Maybe (Maybe (b))
full schema: (tau2 -> Maybe (b)) -> tau2 -> Maybe (b)
full schema: (tau5 -> Maybe (tau2 -> Maybe (b))) -> tau5 -> Maybe (tau2 -> Maybe (b))
full schema: (tau4 -> tau2 -> Maybe (b)) -> tau4 -> tau2 -> Maybe (b)
full schema: (tau6 -> tau4 -> tau2 -> Maybe (b)) -> tau6 -> tau4 -> tau2 -> Maybe (b)
full schema: (tau1 -> b) -> tau1 -> b
before guard Data.Maybe.fromJust arg0
unifying $, rest :: tau1 -> b, goal :: b, result = False
program <- dfs ... goal is "tau1"

fillArgs rest (tau1 -> b) quota' (1)
full schema: (tau5 -> Maybe (Maybe (tau1 -> b))) -> tau5 -> Maybe (Maybe (tau1 -> b))
full schema: (tau4 -> Maybe (tau1 -> b)) -> tau4 -> Maybe (tau1 -> b)
full schema: (tau6 -> tau4 -> Maybe (tau1 -> b)) -> tau6 -> tau4 -> Maybe (tau1 -> b)
full schema: (tau3 -> tau1 -> b) -> tau3 -> tau1 -> b
full schema: (tau6 -> Maybe (tau3 -> tau1 -> b)) -> tau6 -> Maybe (tau3 -> tau1 -> b)
full schema: (tau5 -> tau3 -> tau1 -> b) -> tau5 -> tau3 -> tau1 -> b
full schema: (tau7 -> tau5 -> tau3 -> tau1 -> b) -> tau7 -> tau5 -> tau3 -> tau1 -> b

-}
        -- liftIO $ printf "goal: %s\tsplitting up args for function %s: %s\n" (show goalType) id (show schema)
        -- collect all the argument types (the holes ?? we need to fill)
        -- let args = allArgTypes schema :: [SType]
        -- let args = namedArgTypes schema :: [(Id, SType)]

        -- let fillArg :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
        --     fillArg (quota', programs) arg@(ScalarT _ _) = do
              
        --       program <- dfs env messageChan quota' arg
        --       return (quota' - sizeOf program, programs ++ [program])
            
        --     fillArg (quota', programs) arg = do -- arg is a function type, e.g. a -> Bool
        --       -- program <- (synthesizeProgram2 quota' arg) `mplus` (synthesizeProgram1 quota' arg)
        --       program <- (synthesizeProgram2 quota' arg)
        --       let quota' = quota - sizeOf program
        --       guard (quota' >= 0)
        --       return (quota', programs ++ [program])

        -- (_, argsFilled) <- foldM fillArg (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])
        argsFilled <- fillArgs schema (quota - 1)
        let retProgram = Program { content = PApp id argsFilled, typeOf = refineTop env schema } 
        -- when ("fromJust" `isInfixOf` id) $ liftIO $ printf "retProgram: %s\n" (show retProgram)
        when ("$" `isInfixOf` id) $ liftIO $ printf "retProgram: %s\n" (show retProgram)
        -- liftIO $ printf "quota %d, goal %s, program returned: %s\n" quota (show goalType) (show retProgram)
        guard (sizeOf retProgram <= quota)
        return retProgram

-- !!!! quota 4, goal tau5 -> tau3 -> tau1 -> b, component is ("(Data.Function.$)",<b> . <a> . (((a -> b)) -> (a -> b))), unified to (tau7 -> tau5 -> tau3 -> tau1 -> b) -> tau7 -> tau5 -> tau3 -> tau1 -> b
-- !!!! quota 4, goal Maybe (Maybe (Maybe (b))), component is ("(Data.Function.$)",<b> . <a> . (((a -> b)) -> (a -> b))), unified to (tau4 -> Maybe (Maybe (Maybe (b)))) -> tau4 -> Maybe (Maybe (Maybe (b)))
  where

    -- get the first argument and return the rest
    getFirstArg :: TypeSkeleton r -> Maybe (TypeSkeleton r, TypeSkeleton r)
    getFirstArg (FunctionT x tArg tRes) = Just (tArg , tRes)
    getFirstArg _ = Nothing

    
    -- split up goal: a -> b -> c
    -- so we search for c with a,b in environment
    synthesizeProgram1 :: Int -> SType -> TopDownSolver IO RProgram
    synthesizeProgram1 quota goal = do
      let newArgs = namedArgTypes goal :: [(Id, SType)]
      let (newEnv, retType) = updateEnvWithSpecArgs (refineTop env goal) env :: (Environment, RType)
      body <- dfs newEnv messageChan quota (shape retType) -- call dfs with this function as a goal
      let program = mkLambda newArgs body
      return program

    -- we don't split up goal: a -> b -> c
    -- so we search for a -> b -> c directly
    synthesizeProgram2 :: Int -> SType -> TopDownSolver IO RProgram
    synthesizeProgram2 quota goal = do
      program <- dfs env messageChan quota goal -- call dfs with this function as a goal
      return program
      

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

    -- moves all Data.Function functions to the end and moves the args to the front
    reorganizeSymbols :: [(Id, RSchema)]
    reorganizeSymbols = args ++ withoutDataFunctions ++ dataFunctions ++ dollar
      where
        ogSymbols                             = Map.toList $ env ^. symbols
        (dollar, withoutDollar)               = partition (("$" `isInfixOf`) . fst) ogSymbols
        (_, withoutAnd)                       = partition (("&" `isInfixOf`) . fst) withoutDollar
        (args, withoutArgs)                   = partition (("arg" `isInfixOf`) . fst) withoutAnd
        (dataFunctions, withoutDataFunctions) = partition (("Data.Function" `isInfixOf`) . fst) withoutArgs

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns a reified version of schema (no type vars) w/ sub if unifies 
    getUnifiedComponent :: (Id, RSchema) -> TopDownSolver IO (Id, SType)
    getUnifiedComponent (id, schema) = do
      -- helper fn for guards
      let guardExclude = guard . not . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
      let guardInclude = guard . any (`isInfixOf` id) :: [String] -> TopDownSolver IO ()
      -- guardExclude ["List", "fix", "Data.Tuple"]
      guardInclude ["$", "arg", "fromJust"]

      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType
      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      st' <- get

-- comparing (a) to ("arg1",a)
-- shape (lastType freshVars): a, goalType: a, result: False
-- doing dfs on: tau5 -> Maybe (Maybe (tau1 -> b))

      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      when ((show goalType) == "a" && id == "arg1") $ liftIO $ printf "shape (lastType freshVars): %s, goalType: %s, result: %s\n" (show t1) (show t2) (show checkResult)
      
      guard checkResult
      -- liftIO $ printf "freshvars: %s, shape (lastType freshVars): %s, goalType: %s\n" (show freshVars) (show t1) (show t2)
      -- when ("fromJust" `isInfixOf` id) $
        -- liftIO $ printf "-------\noverall goal: %s\t component: %s (freshvars: %s) \t====> %s\n-------\n" (show t2) (show t1) (show freshVars) (show $ stypeSubstitute sub (shape freshVars))

      return (id, stypeSubstitute sub (shape freshVars))

-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
sizeOf p = sizeOf' p -- + (sizeOfType $ typeOf p)
  where
    -- doesn't consider the size of the type
    sizeOf' :: RProgram -> Int
    sizeOf' p = case content p of
        PSymbol _       -> 1
        PApp _ ps       -> 1 + sum (map sizeOf' ps)
        PFun _ p1       -> 1 + sizeOf' p1
        _               -> error $ "sizeOf doesn't support this thing: " ++ (show p)

sizeOfType :: TypeSkeleton r -> Int
-- sizeOf = length . words . show
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

