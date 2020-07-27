{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Synthesize(synthesize, envToGoal, syn, syn', sizeOf) where

import HooglePlus.TypeChecker
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
    return program


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m =
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO RProgram
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan $ (`map` [5..20]) $ \quota -> do
  liftIO $ printf "running dfs on %s at size %d\n" (show goal) quota

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

    -- regular (a) goaltype
    let program1 = do
          -- program <- dfs env messageChan quota' arg
          -- let quota'' = quota' - sizeOf program
          -- return (quota'', program) :: TopDownSolver IO (Int, RProgram)


          -- liftIO $ printf "goalType: %s\n" (show goalType)
          -- collect all the component types (which we might use to fill the holes)
          component <- choices $ Map.toList (env ^. symbols)

          -- make sure only $ unifies at quota >= 5
          -- but allow anything at quota < 5
          -- guard $ quota < 5 || fst component == "Data.Function.$"
          
          -- stream of components that unify with goal type
          (id, schema) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
          
          -- liftIO $ printf "goalType: %s, id: %s (%s)\n" (show goalType) id (show schema) 
          -- stream of solutions to the (id, schema) returned from getUnifiedComponent
          if isGround schema
            then return Program { content = PSymbol id, typeOf = refineTop env schema }
            else do
              -- collect all the argument types (the holes ?? we need to fill)
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
                    program <- dfs env messageChan quota' arg
                    return (quota' - sizeOf program, programs ++ [program])

                    -- liftIO $ printf "\t* arg: %s\n" (show arg)
                    -- if isFunctionType arg -- arg is e.g. a -> Bool
                      -- then do
                        
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
                        --       -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
                        --       let newArgs = fnTypeArgs arg :: [(Id, SType)]

                        --       let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env arg) env :: (Environment, RType)

                        --       body <- dfs newEnv messageChan quota' (shape newQuery)
                        --       -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
                        --       -- construct the program itself
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

              return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 


    -- call dfs with b, with (a) as argument in env
    let program2 = do
          if isFunctionType goalType
            then do
              -- turns (arg2:a -> arg3:b -> c) into [("arg2",a), ("arg3",b)]
              let newArgs = fnTypeArgs goalType :: [(Id, SType)]

              let (newEnv, newQuery) = updateEnvWithSpecArgs (refineTop env goalType) env :: (Environment, RType)

              body <- dfs newEnv messageChan quota (shape newQuery)
              -- arg: tau4 -> tau2, newArgs:  [("arg2",tau4)], newQuery: tau2
              -- construct the program itself
              let program = mkLambda newArgs body
              let quota' = quota - sizeOf program
              guard (quota' >= 0)
              return program
            else do
              mzero
  
    program <- program1 `interleave` program2
    liftIO $ printf "goal: %s\t\tprogram: %s\n" (show goalType) (show program)
    return program


  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- turn a function type to a list of its arguments
    fnTypeArgs :: SType -> [(Id, SType)]
    fnTypeArgs (FunctionT id t body) = (id, t) : fnTypeArgs body
    fnTypeArgs _ = []
    
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
          
      -- when (id == "Data.List.map") $ liftIO $ printf "id: %s (%s)\n" id (show schema) 
      -- guard ( id == "GHC.List.map" || "Pair" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ("Pair" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- guard ( id == "GHC.List.foldr" || "$" `isInfixOf` id || "arg" `isPrefixOf` id )
      guard ( id == "Data.Tuple.fst" || id == "Data.Tuple.snd" || "$" `isInfixOf` id || "arg" `isPrefixOf` id )

      -- \arg0 ->
      --   (Data.Tuple.fst arg0 (Data.Tuple.snd arg0)) :: a -> b
      --   $
      --   (Data.Tuple.snd arg0) :: b


      -- GHC.List.foldr (\acc x -> []) [] arg0
      -- \xs x -> Data.List.foldr ($) x xs
      -- \xs x -> Data.List.foldr (\a b -> a $ b) x xs
      -- \xs x -> Data.List.foldr (\arg2 -> (\arg3 -> arg2 $ arg3)) arg1 arg0
      -- - name: pipe
      -- query: "[(a -> a)] -> (a -> a)"
      -- solution: "\\xs x -> Data.List.foldr ($) x xs"
      -- source: "hoogle"
      -- example:
      -- syn' "[(a -> a)] -> (a -> a)" [(["[\\x -> x + 1, \\x -> x * 2, \\x -> x * x]", "3"], "19")]
      -- size 10/10 solution: ((Data.Function.$) (GHC.List.foldr (\arg3 ->
      --                   (Data.Function.$) ((Data.Function.$) (\arg2 ->
      --                                          arg4))) arg1)) $ arg0
    --   (\arg2 -> GHC.List.foldr (\arg3 -> \arg4 -> arg1) arg1 arg2) $ arg0
      
      
      
      
      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      st' <- get
      
      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      
      guard checkResult
      return (id, stypeSubstitute sub (shape freshVars))

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
