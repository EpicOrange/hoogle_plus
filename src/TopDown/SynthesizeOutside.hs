{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.SynthesizeOutside(synthesize, envToGoal, synO, synO', sizeOf) where

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
synO :: String -> IO RProgram
synO inStr = synO' inStr []

-- usage:
-- :{
-- syn' "(a -> b) -> (a -> c) -> a -> (b, c)" $ [
--   [ (["\\x -> x + 1", "\\x -> x * 3", "3"], "(4, 9)")
--   , (["\\x -> x ++ x", "Data.List.reverse", "[1,2,3]"], "([1,2,3,1,2,3], [3,2,1])")
--   ]
-- :}
synO' :: String -> [([String], String)] -> IO RProgram
synO' inStr ex = do
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

    -- let arg0Type = toMonotype $ (envWithHo ^. symbols) Map.! "arggggg0" :: RType
    -- let (ScalarT (DatatypeT _ [fnType] _) _) = arg0Type
    -- let (FunctionT arg0argname _ _) = fnType
    -- printf "arg2????????: _%s_\n" arg0argname

    printf "Goal: %s\n" (show goal)
    putStrLn "=================="
    -- syn "(Int -> Bool) -> ((Int -> Bool) -> Char) -> Char" 
    -- expected: \arg0 arg1 -> arg1 arg0
-- - name: applyPair
--   query: "(a -> b, a) -> b"
--   solution: "\\arg0 -> (Data.Tuple.fst arg0) $ (Data.Tuple.snd arg0)"
--   source: "stackOverflow"
--   example:
--     - syn' "(a -> b, a) -> b" [(["(\\x -> x * x, 10)"], "100")]
--   goal is b, use $
--      a -> b
--      a
    -- call dfs with iterativeDeepening
    program <- iterativeDeepening envWithHo messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    -- return program
    return undefined


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m =
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan $ (`map` [1..20]) $ \quota -> do
  liftIO $ printf "\nrunning dfs on %s at size %d\n" (show goal) quota

  let goalType = shape $ lastType $ toMonotype goal :: SType
  solution <- dfs env messageChan quota goalType :: TopDownSolver IO RProgram
  
  -- check if the program has all the arguments that it should have (avoids calling check)
  -- liftIO $ printf "size %d/%d solution: %s\n" (sizeOf solution) quota (show solution)
  
  liftIO $ printf "found... %s \n" (show solution)

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
    -- liftIO $ printf "\tdfs'ing the following goal: %s\n" (show goalType)
    when (isFunctionType goalType) $ liftIO $ printf "omg it's function type: %s \n" (show goalType)

    -- regular (a) goaltype
    let program1 = do
          -- liftIO $ printf "goalType: %s\n" (show goalType)
          -- stream of components that unify with goal type (which we might use to fill the holes)
          component <- choices $ Map.toList (env ^. symbols)
          
          -- replace type vars with 
          (id, freshVars) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
          -- if ("$" `isInfixOf` id) 
          --   then liftIO $ printf "\tdoing stuff for: %s\n" id 
          --   else liftIO $ printf "\tdoing stuff for ****** other\n" 
          
          st' <- get
          let sub = st' ^. typeAssignment
          let schema' = stypeSubstitute sub freshVars

          -- liftIO $ printf "-------\ngoalType: %s \t======> (%s \t%s)\n-------\n" (show goalType) id (show schema)
          
          -- stream of solutions to the (id, schema) returned from getUnifiedComponent
          if isGround schema'
            then do
              -- liftIO $ printf "heere\n"
              return Program { content = PSymbol id, typeOf = refineTop env schema' }
            else do

              -- liftIO $ printf "heere2\n"
              -- collect all the argument types (the holes ?? we need to fill)
              
              -- TODO go through this and check if it's right
              -- let args = allArgTypes schema' :: [SType]
              -- Maybe (tau1) -> tau1
              -- [(a->b)] -> (a->b)
              -- args: [tau1]
              let args = allArgTypes freshVars :: [SType]
              when (id == "Data.Maybe.fromJust") $ do
                let args = allArgTypes schema' :: [SType]   -- [tau]
                liftIO $ printf "args: _%s_\n" (show args)
                let (arg0Type:xs) = args
                -- when fromJust unifies to be type Maybe ( arg2: tau1 -> b) -> tau1 -> b
                -- i want the name of the tau1               ^
                case arg0Type of
                  (ScalarT (DatatypeT _ [fnType] _) _) -> do
                    case fnType of
                       (FunctionT arg0argname _ _) -> liftIO $ printf "arg2????????: _%s_\n" arg0argname
                       _ -> return ()
                  _ -> return ()

              -- freshVars: [("arg0",arg2:tau5 -> tau4),("arg1",tau5)]
              --- arg0:Maybe (arg2:a -> b) -> arg1:a -> b
              liftIO $ printf "\t 0000: env has %s\n" (show $ Map.filterWithKey (\k v -> "arg" `isPrefixOf` k) $ env ^. symbols) 

              -- liftIO $ printf "goal: %s\tsplitting up args for function %s: %s, args: %s\n" (show goalType) id (show schema') (show args)
              -- liftIO $ printf "goal: %s\tsplitting up args for function %s: %s, args: %s\n" (show goalType) id (show freshVars) (show args)

              -- let args = namedArgTypes schema :: [(Id, SType)]

              let func :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
                  func (quota', programs) arg
                    
                    | not (isFunctionType arg) = do -- regular argument
                        
                        let typedArg = stypeSubstitute sub arg
                        -- when ("$" `isInfixOf` id) $ liftIO $ printf "\tturning the following arg (%s) into (%s)\n" (show arg) (show typedArg)
                        program <- dfs env messageChan quota' typedArg
                        return (quota' - sizeOf program, programs ++ [program])
                    
                    | otherwise = do -- arg is e.g. a -> Bool
                        let typedArg = stypeSubstitute sub arg


-- newEnv: [...("snd",<b> . <a> . ((a , b) -> b)),("fst",<b> . <a> . ((a , b) -> a)),("arg1",a),("arg0",Maybe (((a -> b)))),("Text.Show.showsPrec",<a> . (@@hplusTC@@Show (a) -> (Int -> (a -> ([Char] -> [Char])))))]



                        -- liftIO $ printf "\tturning the following arg (%s) into (%s)\n" (show arg) (show typedArg)
                        -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
                        -- liftIO $ printf "\tenv before has %s, newEnv has %s\n" (show $ filter ("arg" `isPrefixOf`) $ Map.keys $ env ^. symbols) (show $ filter ("arg" `isPrefixOf`) $ Map.keys $ newEnv ^. symbols)

                        let newArgs = namedArgTypes arg :: [(Id, SType)] -- [Maybe tau1]
                        let newArgsTyped = map (\(id, schema) -> (id, stypeSubstitute sub schema)) newArgs :: [(Id, SType)]
                        let newArgsTyped' = ("argblahhhhhhhhhhhh", ScalarT (DatatypeT "Bool" [] []) ()) : newArgsTyped
                        let retType = stypeSubstitute sub $ lastType arg :: SType

                        -- add these args as components
                        -- TODO this is where the problem is bummer!!!! 
                        -- TODO idea: rewrite this to not use updateEnvWithSpecArgs
                        --   but instead manually do what updateEnvWithSpecArgs does
                        --   and in the end, get newArgsTyped into the env
                        --   (hint: look at:
                        --        addVariable x tArg $ addArgument x tArg env)

                        -- Maybe (tau1) -> tau1
                        -- (Maybe( a -> b)) -> (a -> b) 
                          -- newArgs: [("arg2",tau1)], newArgsTyped: [("arg2",tau1)], retType: tau0, arg: tau1 -> tau0, typedArg: tau1 -> b
                          -- difference: fromList []
                        -- let newEnv = foldr (\(x, t) env -> addArgument x (refineTop env t) env) env newArgsTyped
                        let newEnv = foldr (\(x, t) env' -> addVariable x (refineTop env' t) env') env newArgsTyped'
                        -- liftIO $ printf "\tnewArgs: %s, newArgsTyped: %s, retType: %s, arg: %s, typedArg: %s\n" (show newArgs) (show newArgsTyped) (show retType) (show arg) (show typedArg)
                        -- liftIO $ printf "\tfreshVars: %s\n" (show $ namedArgTypes $ freshVars)
                        -- liftIO $ printf "\tcontains symbols old: %s: %s\n" (show (take 1 newArgsTyped)) (show (Map.member (fst (newArgsTyped !! 0)) (env ^. symbols)))
                        -- liftIO $ printf "\tcontains symbols new: %s: %s\n" (show (take 1 newArgsTyped)) (show (Map.member (fst (newArgsTyped !! 0)) (newEnv ^. symbols)))
                        -- liftIO $ printf "\tcontains args: %s: %s\n" (show (take 1 newArgsTyped)) (show (Map.member (fst (newArgsTyped !! 0)) (newEnv ^. arguments)))
                        -- liftIO $ printf "\tenv has %d things, newEnv has %d things\n" (Map.size $ env ^. arguments) (Map.size $ newEnv ^. arguments)
                        liftIO $ printf "\tenv has %s, newEnv has %s\n" (show $ filter ("arg" `isPrefixOf`) $ Map.keys $ env ^. symbols) (show $ filter ("arg" `isPrefixOf`) $ Map.keys $ newEnv ^. symbols)
                        -- liftIO $ printf "\tdifference: %s\n" (show $ Map.difference (newEnv ^. arguments) (env ^. arguments))

                        -- let (newEnv, newQuery) = updateEnvWithSpecArgs' (refineTop env typedArg) env' :: (Environment, RType)
        --                         newArgs: [("arg2",tau8)], newArgsTyped: [("arg2",tau8)], retType: Maybe (b), arg: tau8 -> tau7, typedArg: tau8 -> Maybe (b)
        -- freshVars: [("arg0",tau8 -> tau7),("arg1",tau8)]
        -- contains symbols: [("arg2",tau8)]: True
        -- contains args: [("arg2",tau8)]: False
        -- env has 2 things, newEnv has 2 things
                        -- let newSymbols = Map.toList $ newEnv ^. symbols

                        -- let matchesIt :: Id -> [(Id, SType)] -> Bool
                        --     matchesIt _  []          = False
                        --     matchesIt id ((x, _):xs) = id == x || matchesIt id xs

                        -- let mapF :: (Id, RSchema) -> (Id, RSchema)
                        --     mapF c@(id, schema) = if (matchesIt id newArgs) then (id, stypeSubstitute sub (shape schema))
                        --                                                     else c
                        -- let newSymbols' = map filterF newSymbols
                        
                        body <- dfs newEnv messageChan quota' retType
                        
                        -- construct the program itself
                        let program = mkLambda newArgs body
                        let quota'' = quota' - sizeOf program
                        guard (quota'' >= 0)
                        return (quota'', programs ++ [program]) 
                        
                    -- liftIO $ printf "\t* arg: %s\n" (show arg)

              (_, argsFilled) <- foldM func (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])
              return Program { content = PApp id argsFilled, typeOf = refineTop env schema' } 


    -- call dfs with b, with (a) as argument in env
    let program2 = do
        -- mzero
          if isFunctionType goalType
            then do
              -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
              let newArgs = namedArgTypes goalType :: [(Id, SType)]

              let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env goalType) env :: (Environment, RType)

              body <- dfs newEnv messageChan (quota - 1) (shape newQuery)
              -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
              -- construct the program itself
              let program = mkLambda newArgs body :: RProgram
              let quota' = quota - sizeOf program
              guard (quota' >= 0)

        --       liftIO $ printf "newArgs: %s, body: %s\n" (show newArgs) (show body)
              -- liftIO $ printf "id %s, program: %s\n" id (show program)

              return program
            else do
              mzero
  
    -- program <- program1 `interleave` program2
    program <- program1
    -- liftIO $ printf "goal: %s\t\tprogram: %s\n" (show goalType) (show program)
    return program

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
    mkLambda args body =
      -- let (body', args') = etareduce (body, args)
      -- in foldr addArg body' args'
      foldr addArg body args

      where
        -- remove unnecessary lambdas: (\arg1 -> arg0 arg1) becomes (arg0)
        -- TODO need to make sure arg1 not used anywhere else in body
        etareduce :: (RProgram, [(Id, SType)]) -> (RProgram, [(Id, SType)])
        etareduce (Program (PApp id xs) t, args)
          | not (null xs || null args),
            (ps, Program (PSymbol p) _) <- (init xs, last xs),
            (as, (a, _))                <- (init args, last args),
            p == a
          = etareduce (Program (PApp id ps) t, as)
        etareduce p = p

        -- add the given argument to body
        addArg :: (Id, SType) -> RProgram -> RProgram
        addArg (id',t') body''@(Program _ bodyType) =
          Program { 
            content = PFun id' body'', 
            typeOf = FunctionT id' (refineTop env t') bodyType
          }

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns updated schema w/ sub if unifies 
    getUnifiedComponent :: (Id, RSchema) -> TopDownSolver IO (Id, SType)
    getUnifiedComponent (id, schema) = do
        --   \arg0 arg1 -> Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg1) arg0
      -- when (id == "Data.List.map") $ liftIO $ printf "id: %s (%s)\n" id (show schema) 
      -- guard ( id == "GHC.List.map" || "Pair" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ("Pair" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ( id == "GHC.List.foldr" || "$" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ( id == "Data.Tuple.fst" || id == "Data.Tuple.snd" || "$" `isInfixOf` id || "arg" `isPrefixOf` id )
      guard ( id == "Data.Maybe.fromJust" || "$" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ( id == "Data.Maybe.Just" || id == "Data.Bool.bool"  || id == "Data.Maybe.Nothing" || "arg" `isPrefixOf` id )



      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema :: TopDownSolver IO RType



      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      st' <- get
      
    --   let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      
      -- when ("$" `isInfixOf` id) $ liftIO $ printf "%s\n" (show freshVars) -- "-------\nt1: %s\t t2: %s\t====> %s\n-------\n" (show t1) (show t2) (show checkResult)
      -- when ("$" `isInfixOf` id && (show t2) == "b") $ liftIO $ printf "-------\nt1: %s\t t2: %s\t====> %s\n-------\n" (show t1) (show t2) (show checkResult)
      -- when ("fromJust" `isInfixOf` id) $ liftIO $ printf "-------\ngoalType: %s \t====> (%s \t%s) \t====> %s\n-------\n" (show goalType) id (show schema) (show checkResult)

      guard checkResult
      -- when ("fromJust" `isInfixOf` id) $ liftIO $ printf "-------\noverall goal: %s\t component: %s (freshvars: %s) \t====> %s\n-------\n" (show t2) (show t1) (show freshVars) (show $ stypeSubstitute sub (shape freshVars))

    --   return (id, stypeSubstitute sub (shape freshVars))
      return (id, (shape freshVars))

-- gets the size of a program, used for checking quota
sizeOf :: RProgram -> Int
-- sizeOf = length . words . show
sizeOf p =
  case content p of
    PSymbol _       -> 1
    PApp _ ps       -> 1 + sum (map sizeOf ps)
    PFun _ p1       -> 1 + sizeOf p1
    PIf p1 p2 p3    -> sizeOf p1 + sizeOf p2 + sizeOf p3
    PMatch p1 cases -> sizeOf p1 + sum (map (sizeOf . expr) cases)
    PFix _ p1       -> sizeOf p1
    PLet _ p1 p2    -> sizeOf p1 + sizeOf p2
    _               -> 0







{-
-- goal: Maybe (Maybe (Maybe (b)))         program: arg2
-- goal: Maybe (Maybe (b))         program: Data.Maybe.fromJust arg2
-- goal: Maybe (b)         program: Data.Maybe.fromJust (Data.Maybe.fromJust arg2)
-- goal: b         program: Data.Maybe.fromJust (Data.Maybe.fromJust (Data.Maybe.fromJust arg2))
-- goal: b         program: 
-- (\arg2 -> Data.Maybe.fromJust (Data.Maybe.fromJust (Data.Maybe.fromJust arg2))) $ arg2

      {-
        syn "arg0:Maybe (a -> b) -> arg1:a -> b"
        (fromJust arg0) arg1

                                            ?? :: b
                                               \
                              $ ?? :: (tau1 -> b) -> (?? :: a) -> b
                                  /                           \
              fromJust ?? :: Maybe (tau1 -> b) -> (a -> b)   arg1 :: a
                              /
                          arg0  :: Maybe (a -> b)


        solution:       (Data.Maybe.fromJust arg0) $ arg1
        we got:         (Data.Maybe.fromJust arg0 arg1) $ arg1
        we prob want:   (\arg2 -> Data.Maybe.fromJust arg0 arg2) $ arg1
        
        
        
        overall goal: b  component: tau4 (freshvars: (Maybe (tau4) -> tau4))    ====> Maybe (b) -> b
        overall goal: tau1 -> b  component: tau4 (freshvars: (Maybe (tau4) -> tau4))    ====> Maybe (tau1 -> b) -> tau1 -> b

        their solution: (Data.Maybe.fromJust arg0) $ arg1
        starting goal type: b
        component: arg0     :: Maybe (a -> b)
        component: fromJust :: Maybe tau1 -> tau1
        component: $        :: (((tau1 -> tau2)) -> (tau1 -> tau2))

        component: fromJust :: Maybe (tau1 -> tau2) -> (tau1 -> tau2)

        currently, we only check this:
          unified: fromJust :: Maybe b -> b
        but really we wanna check this too:
          unified: fromJust :: Maybe (tau1 -> b) -> (tau1 -> b)
        
      -}

--
-- does DFS stuff, with max size of program given as a quota
--
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownSolver IO RProgram
dfs env messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = do

    -- liftIO $ printf "\tdfs'ing the following goal: %s\n" (show goalType)

    -- regular (a) goaltype
    let program1 = do
          -- program <- dfs env messageChan quota' arg
          -- let quota'' = quota' - sizeOf program
          -- return (quota'', program) :: TopDownSolver IO (Int, RProgram)


          -- liftIO $ printf "goalType: %s\n" (show goalType)
          -- collect all the component types (which we might use to fill the holes)
          component <- choices $ Map.toList (env ^. symbols)

          -- stream of components that unify with goal type
          (id, schema) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
          
          -- liftIO $ printf "-------\ngoalType: %s \t======> (%s \t%s)\n-------\n" (show goalType) id (show schema)

          -- goalType: tau1 -> Maybe (a)     ======> (GHC.List.sum   @@hplusTC@@Num (tau1 -> Maybe (a)) -> [tau1 -> Maybe (a)] -> tau1 -> Maybe (a))
          --  tau1 -> tau2
          -- problem: tau1 (as a goal) will never unify with (a->b)
          
          -- stream of solutions to the (id, schema) returned from getUnifiedComponent
          if isGround schema
            then return Program { content = PSymbol id, typeOf = refineTop env schema }
            else do

              liftIO $ printf "goal: %s\tsplitting up args for function %s: %s\n" (show goalType) id (show schema)
              -- collect all the argument types (the holes ?? we need to fill)
              -- let args = namedArgTypes schema :: [(Id, SType)]
              let args = allArgTypes schema :: [SType]
              -- .$ unified
              -- args: [ (a -> b), b]
              -- liftIO $ printf "goalType: %s, id: %s (%s)\n" (show goalType) id (show schema) 

              -- do basically this:
              -- dfsstuff0 <- dfs ... arg0 (quota - 1) :: RProgram
              -- dfsstuff1 <- dfs ... arg1 (quota - 1 - sizeOf dfsstuff0) :: RProgram
              -- dfsstuff2 <- dfs ... arg2 (quota - 1 - sizeOf dfsstuff0 - sizeOf dfsstuff1) :: RProgram
              -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
              let func :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
                  func (quota', programs) arg = do
                    -- program <- dfs env messageChan quota' arg
                    liftIO $ printf "\tdoing stuff for arg: %s\n" (show arg)
                    if isFunctionType arg -- arg is e.g. a -> Bool
                      then do
                  --       -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
                        let newArgs = namedArgTypes arg :: [(Id, SType)]
                        -- liftIO $ printf "newArgs: %s\n" (show newArgs)

                        let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env arg) env :: (Environment, RType)

                        body <- dfs newEnv messageChan (quota' - (length newArgs)) (shape newQuery)
                  --       -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
                  --       -- construct the program itself
                        let program = mkLambda newArgs body
                        let quota'' = quota' - sizeOf program
                        guard (quota'' >= 0)
                        return (quota'', programs ++ [program]) 
                        
                    -- liftIO $ printf "\t* arg: %s\n" (show arg)
                      else do
                        program <- dfs env messageChan quota' arg
                        return (quota' - sizeOf program, programs ++ [program])

                        -- return (quota' - sizeOf program, programs ++ [program])
                        
                        -- [("arg2",a)], a -> b -> c
                        -- when (isInfixOf "$" id) $ liftIO $ printf "(%s, %s) => arg: %s, newArgs: %s, newQuery: %s\n" id (show schema) (show arg) (show newArgs) (show newQuery)
                        -- ((Data.Function.$), (a -> b) -> a -> b) => arg: a -> b, newArgs: [("arg2",a)], newQuery: b
                        -- fst :: (a -> b, tau4) -> (a -> b)
                        -- synthesize a -> b   => \arg2 -> ?? :: b
                        -- synthesize a -> b   => \arg2 -> (fst arg0) arg2
                        
                        -- (a -> b) -> (a -> c) -> a -> (b, c)
                        -- \arg0 arg1 arg2 -> ((arg0 arg2) , (arg1 arg2))
                        -------------------
                        -- call dfs with (a -> b)
                        -- TODO move this to the beginning of DFS instead 
                        -- let program1 = do
                        --       program <- dfs env messageChan quota' arg
                        --       let quota'' = quota' - sizeOf program
                        --       return (quota'', program) :: TopDownSolver IO (Int, RProgram)
                        
                        -- -- call dfs with b, with (a) as argument in env
                        -- let program2 = do
                        -- --       -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
                        --       let newArgs = namedArgTypes arg :: [(Id, SType)]

                        --       let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env arg) env :: (Environment, RType)

                        --       body <- dfs newEnv messageChan quota' (shape newQuery)
                        -- --       -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
                        -- --       -- construct the program itself
                        --       let program = mkLambda newArgs body
                        --       let quota'' = quota' - sizeOf program
                        --       guard (quota'' >= 0)
                        --       return (quota'', program) :: TopDownSolver IO (Int, RProgram)
                        
                        -- (quota'', program) <- program1 `interleave` program2
                        
                        -- return (quota'', programs ++ [program])
                        -- return undefined 
                        -------------------
                        -- body <- dfs newEnv messageChan quota' (shape newQuery)
                        -- -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
                        -- -- construct the program itself
                        -- let program = mkLambda newArgs body
                        -- let quota'' = quota' - sizeOf program
                        -- guard (quota'' >= 0)

                        
                        -- liftIO $ printf "newArgs: %s, body: %s\n" (show newArgs) (show body)
                        -- liftIO $ printf "id %s, program: %s\n" id (show program)

                        -------------------
                      -- else do
                          -- program <- dfs env messageChan quota' arg
                          -- return (quota' - sizeOf program, programs ++ [program])

              (_, argsFilled) <- foldM func (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])

              -- when (id == "Data.Maybe.fromJust") $
              --   liftIO $ printf "schema: %s\t\targ holes: %s\t\targs found: %s\n" (show schema) (show args) (show argsFilled) 
                
                
                -- schema: Maybe (tau1 -> b) -> tau1 -> b          arg holes: [Maybe (tau1 -> b),tau1]             args found: [arg0,arg1]
                -- schema: Maybe tau2 -> Tau2
                -- sum :: Int -> (Int -> Int)
                -- (\arg2 arg3 -> sum 1 2 )
                
                -- what we get rn: (\arg2 -> fromJust arg0 arg2) $ arg1
                  -- ==================
                  -- running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 1
                  -- running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 2
                  -- running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 3
                  -- goal: Maybe (tau1 -> b)         program: arg0
                  -- running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 4
                  -- goal: Maybe (tau1 -> b)         program: arg0
                  -- goal: tau1              program: arg1
                  -- schema: Maybe (tau1 -> b) -> tau1 -> b          arg holes: [Maybe (tau1 -> b),tau1]             args found: [arg0,arg1]
                  -- goal: tau1 -> b         program: Data.Maybe.fromJust arg0 arg1
                  -- running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 5
                  -- goal: Maybe (tau1 -> b)         program: arg0
                  -- goal: tau1              program: arg1
                  -- schema: Maybe (tau1 -> b) -> tau1 -> b          arg holes: [Maybe (tau1 -> b),tau1]             args found: [arg0,arg1]
                  -- goal: tau1 -> b         program: Data.Maybe.fromJust arg0 arg1
                  -- goal: tau1              program: arg1
                  -- goal: b         program: (Data.Maybe.fromJust arg0 arg1) $ arg1
                  -- found... (Data.Maybe.fromJust arg0 arg1) $ arg1 

              return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 


    -- -- call dfs with b, with (a) as argument in env
    -- let program2 = do
    --     -- mzero
    --       if isFunctionType goalType
    --         then do
    --           -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
    --           let newArgs = namedArgTypes goalType :: [(Id, SType)]

    --           let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env goalType) env :: (Environment, RType)

    --           body <- dfs newEnv messageChan (quota - 1) (shape newQuery)
    --           -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
    --           -- construct the program itself
    --           let program = mkLambda newArgs body :: RProgram
    --           let quota' = quota - sizeOf program
    --           guard (quota' >= 0)

    --           liftIO $ printf "newArgs: %s, body: %s\n" (show newArgs) (show body)
    --           -- liftIO $ printf "id %s, program: %s\n" id (show program)

    --           return program
    --         else do
    --           mzero
  
    -- program <- program1 `interleave` program2
    program <- program1
    liftIO $ printf "goal: %s\t\tprogram: %s\n" (show goalType) (show program)
    return program





-}


{-


running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 3
goal: b splitting up args for function (Data.Function.$): (tau1 -> b) -> tau1 -> b
        doing stuff for arg: tau1 -> b
goal: b splitting up args for function (Data.Function.$): (tau3 -> b) -> tau3 -> b
        doing stuff for arg: tau3 -> b
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: b         program: arg2
        doing stuff for arg: tau1
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau2 -> Maybe (b)) -> tau2 -> Maybe (b)
        doing stuff for arg: tau2 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function (Data.Function.$): (tau3 -> Maybe (Maybe (b))) -> tau3 -> Maybe (Maybe (b))
        doing stuff for arg: tau3 -> Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (Maybe (b))) -> Maybe (Maybe (b))
        doing stuff for arg: Maybe (Maybe (Maybe (b)))

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 4
goal: b splitting up args for function (Data.Function.$): (tau1 -> b) -> tau1 -> b
        doing stuff for arg: tau1 -> b
goal: b splitting up args for function (Data.Function.$): (tau3 -> b) -> tau3 -> b
        doing stuff for arg: tau3 -> b
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (b)         program: arg2
goal: b         program: Data.Maybe.fromJust arg2
        doing stuff for arg: tau1
goal: b         program: arg2
        doing stuff for arg: tau1
goal: tau1      splitting up args for function (Data.Function.$): (tau3 -> b) -> tau3 -> b
        doing stuff for arg: tau3 -> b
goal: tau1      splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau2 -> Maybe (b)) -> tau2 -> Maybe (b)
        doing stuff for arg: tau2 -> Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (b)         program: arg2
        doing stuff for arg: tau2
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function (Data.Function.$): (tau3 -> Maybe (Maybe (b))) -> tau3 -> Maybe (Maybe (b))
        doing stuff for arg: tau3 -> Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (Maybe (b))) -> Maybe (Maybe (b))
        doing stuff for arg: Maybe (Maybe (Maybe (b)))
goal: Maybe (Maybe (Maybe (b))) splitting up args for function (Data.Function.$): (tau4 -> Maybe (Maybe (Maybe (b)))) -> tau4 -> Maybe (Maybe (Maybe (b)))
        doing stuff for arg: tau4 -> Maybe (Maybe (Maybe (b)))
goal: Maybe (Maybe (Maybe (b))) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (Maybe (Maybe (b)))) -> Maybe (Maybe (Maybe (b)))
        doing stuff for arg: Maybe (Maybe (Maybe (Maybe (b))))

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 5
goal: b splitting up args for function (Data.Function.$): (tau1 -> b) -> tau1 -> b
        doing stuff for arg: tau1 -> b
goal: b splitting up args for function (Data.Function.$): (tau3 -> b) -> tau3 -> b
        doing stuff for arg: tau3 -> b
goal: b splitting up args for function (Data.Function.$): (tau5 -> b) -> tau5 -> b
        doing stuff for arg: tau5 -> b
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: b         program: arg2
        doing stuff for arg: tau3
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function (Data.Function.$): (tau5 -> Maybe (Maybe (b))) -> tau5 -> Maybe (Maybe (b))
        doing stuff for arg: tau5 -> Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (Maybe (b))) -> Maybe (Maybe (b))
        doing stuff for arg: Maybe (Maybe (Maybe (b)))
goal: Maybe (Maybe (b))         program: arg2
goal: Maybe (b)         program: Data.Maybe.fromJust arg2
goal: b         program: Data.Maybe.fromJust (Data.Maybe.fromJust arg2)
        doing stuff for arg: tau1
goal: Maybe (b)         program: arg2
goal: b         program: Data.Maybe.fromJust arg2
        doing stuff for arg: tau1
goal: tau1      splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: tau1      splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: b         program: arg2
        doing stuff for arg: tau1
goal: tau1      splitting up args for function (Data.Function.$): (tau3 -> b) -> tau3 -> b
        doing stuff for arg: tau3 -> b
goal: tau1      splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: b splitting up args for function Data.Maybe.fromJust: Maybe (b) -> b
        doing stuff for arg: Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau2 -> Maybe (b)) -> tau2 -> Maybe (b)
        doing stuff for arg: tau2 -> Maybe (b)
goal: Maybe (b) splitting up args for function (Data.Function.$): (tau4 -> Maybe (b)) -> tau4 -> Maybe (b)
        doing stuff for arg: tau4 -> Maybe (b)
goal: Maybe (b) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (b)) -> Maybe (b)
        doing stuff for arg: Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function (Data.Function.$): (tau5 -> Maybe (Maybe (b))) -> tau5 -> Maybe (Maybe (b))
        doing stuff for arg: tau5 -> Maybe (Maybe (b))
goal: Maybe (Maybe (b)) splitting up args for function Data.Maybe.fromJust: Maybe (Maybe (Maybe (b))) -> Maybe (Maybe (b))
        doing stuff for arg: Maybe (Maybe (Maybe (b)))
goal: Maybe (Maybe (b))         program: arg2
goal: Maybe (b)         program: Data.Maybe.fromJust arg2



-}




{-

when unifying with fromJust :: <a> . Maybe a -> a
tau1 -> b

fromJust :: Maybe (tau1 -> b) -> tau1 -> b

unify with arg0 :: Maybe (arg2:a -> b)



==================
Starting!
Arguments: fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
Goal: b
==================

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 1
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 2
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 3
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 4
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau2),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]

running dfs on <b> . <a> . (Maybe (((a -> b))) -> (a -> b)) at size 5
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau7),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau7),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau6),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau6),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau6),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau6),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau4),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau5),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau1),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arg2","arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [("arg2",tau3),("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
         0000: env has fromList [("arggggg0",Maybe (((a -> b)))),("arggggg1",a)]
        env has ["arggggg0","arggggg1"], newEnv has ["arg2","arggggg0","arggggg1"]
         0000: env has fromList [^C("arggggg0",MInterrupted.



-}