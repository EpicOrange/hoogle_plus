{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.SynthesizeLogic where
-- module TopDown.SynthesizeLogic(synthesize, envToGoal) where

-- import Database.Environment
-- import Database.Util
-- import qualified HooglePlus.Abstraction as Abstraction
-- import PetriNet.PNSolver
-- import HooglePlus.TypeChecker
-- import HooglePlus.GHCChecker (check)
-- import Synquid.Error
-- import Synquid.Logic
-- import Synquid.Parser
-- import Synquid.Pretty
-- import Synquid.Program
-- import Synquid.Resolver
-- import Synquid.Type
-- import Synquid.Util
-- import Types.CheckMonad
-- import Types.Common
-- import Types.Environment
-- import Types.Experiments
-- import Types.Filtering
-- import Types.Program
-- import Types.Solver
-- import Types.TopDown
-- import Types.TypeChecker
-- import Types.Type
-- import Types.IOFormat
-- import HooglePlus.Utils
-- import HooglePlus.IOFormat
-- import Examples.ExampleChecker
-- import PetriNet.Util

-- import Control.Applicative ((<$>))
-- import Control.Concurrent.Chan
-- import Control.Exception
-- import Control.Lens
-- import Control.Monad
-- import Control.Monad.Except
-- import Control.Monad.Logic
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Data.Either
-- import Data.List
-- import Data.List.Extra (nubOrdOn)
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- import Data.Maybe
-- import qualified Data.Set as Set
-- import Data.Set (Set)
-- import Data.Time.Clock
-- import Data.Time.Format
-- import System.CPUTime
-- import System.Exit
-- import Text.Parsec.Indent
-- import Text.Parsec.Pos
-- import Text.Printf (printf)


-- envToGoal :: Environment -> String -> IO Goal
-- envToGoal env queryStr = do
--   let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
--   let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
--   case parseResult of
--     Left parseErr -> let e = toErrorMessage parseErr
--                       in putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
--     Right (funcDecl:decl:_) -> case decl of
--       Pos _ (SynthesisGoal id uprog) -> do
--         let Pos _ (FuncDecl _ sch) = funcDecl
--         let goal = Goal id env sch uprog 3 $ initialPos "goal"
--         let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
--         case spec of
--           Right sp -> do
--             let (env', monospec) = updateEnvWithBoundTyVars sp env
--             let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
--             return $ goal { gEnvironment = env'', gSpec = sp }
--           Left parseErr -> putDoc (pretty parseErr) >> putDoc linebreak >> error (prettyShow parseErr)
--       _ -> error "parse a signature for a none goal declaration"

-- synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
-- synthesize searchParams goal examples messageChan = do
--     let rawEnv = gEnvironment goal
--     let goalType = gSpec goal :: RSchema
--     let destinationType = lastType (toMonotype goalType)
--     let useHO = _useHO searchParams
--     let rawSyms = rawEnv ^. symbols
--     let hoCands = rawEnv ^. hoCandidates
--     envWithHo <- do
--     --------------------------
--     -- HIGHER ORDER STUFF 
--     -- envWithHo <- if useHO -- add higher order query arguments
--     --     then do
--     --         let args = rawEnv ^. arguments
--     --         let hoArgs = Map.filter (isFunctionType . toMonotype) args
--     --         let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
--     --         return $ rawEnv { 
--     --             _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
--     --             _hoCandidates = hoCands ++ map fst hoFuns
--     --             }
--     --     else do
--     --------------------------

--       let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
--       return $ rawEnv {
--           _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
--           _hoCandidates = []
--           }

--     -- used for figuring out which programs to filter (those without all arguments)
--     let numArgs = length (Map.elems (envWithHo ^. arguments))

--     start <- getCPUTime

--     iterativeDeepening envWithHo messageChan searchParams examples goalType numArgs

--     end <- getCPUTime

--     let diff = fromIntegral (end - start) / (10^12)
--     printf "Computation time: %0.3f sec\n" (diff :: Double)

-- -- ghcCheckedFunction = \thisArg -> sum (sum (last []) []) (take (sum thisArg) (last []))

-- -- sum :: Num a => [a] -> a
-- -- last :: [a] -> a

-- -- GHC.List.sum (GHC.List.last ([] :: [[a]]) ) []
-- -- ------------
    
--     writeChan messageChan (MesgClose CSNormal)
--     return ()

--     -- where
      
-- -- determines if the result has all the appropriate arguments given the number of args
-- filterParams :: Int -> RProgram -> Bool
-- filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
-- filterParams 1       x = "arg0" `isInfixOf` (show x)
-- filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x


-- type CompsSolver m = StateT Comps (StateT CheckerState m)
-- evalCompsSolver messageChan m = m `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- -- type TopDownBackTrack m = LogicT (StateT CheckerState m)
-- -- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- -- -- evalTopDownBackTrack messageChan action = observeAllT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
-- -- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- -- -- evalTopDownBackTrack messageChan action = fmap (\x -> [x]) $ observeT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
-- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m a
-- -- evalTopDownBackTrack messageChan action = observeT action `evalStateT` (emptyChecker { _checkerChan = messageChan })


-- instance Monad m => CheckMonad (CompsSolver m) where
--     getNameCounter = lift getNameCounter
--     setNameCounter = lift . setNameCounter
--     getNameMapping = lift getNameMapping
--     setNameMapping = lift . setNameMapping
--     getIsChecked   = lift getIsChecked
--     setIsChecked   = lift . setIsChecked
--     getMessageChan = lift getMessageChan
--     overStats      = lift . overStats

-- type BTCompsSolver m = LogicT (CompsSolver m)

-- -- try to get solutions by calling dfs on depth 1 2 3 4... until we get an answer

-- -- stack run -- hplus --json='{"query": "arg0:a -> arg1:[Maybe a] -> a", "inExamples": []}'
-- -- arg0:a -> arg1:[Maybe a] -> a

-- -- without CompsSolver
-- -- Data.Maybe.fromMaybe arg0 (GHC.List.last arg1)
-- -- Computation time: 35.623 sec, 34.724 sec, 22.624 sec

-- -- with CompsSolver
-- -- Data.Maybe.fromMaybe arg0 (GHC.List.last arg1)
-- -- Computation time: 26.022 sec, 27.586 sec

-- iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> Int -> IO ()
-- iterativeDeepening env messageChan searchParams examples goal numArgs = do
-- -- solution <- choices []
-- -- solution <- once $ iterativeDeepening 1 `mplus` iterativeDeepening 2 `mplus` ...
-- -- solution <- once $ msum [iterativeDeepening 1, iterativeDeepening 2]
-- -- solution <- once $ msum $ map iterativeDeepening [1..]
--   solution <- evalCompsSolver messageChan $ observeT $ msum $ map helper [1..] :: IO RProgram
--   print solution

--   where
--     helper :: Int -> BTCompsSolver RProgram
--     helper depth = do
--       liftIO $ printf "running dfs on %s at depth %d\n" (show goal) depth

--       -- print size of map
--       comps <- lift get
--       let memoMap = comps ^. memoize :: Map SType [(Id, SType)]
--       liftIO $ print $ Map.size memoMap

--       let goalType = shape $ lastType $ toMonotype goal :: SType
--       solutionLazy <- dfs env messageChan depth goalType :: BTCompsSolver IO RProgram

--       -- solutionLazy <- choices solutions
--       isChecked <- liftIO $ check' solutionLazy
--       guard isChecked -- gets the first valid program

--       -- liftIO $ print solutionLazy
--       return solutionLazy
--     check' :: RProgram -> IO Bool
--     check' program = do
--       if (filterParams numArgs program) 
--         then do
--           -- return True
          
--           printf "omg we are checking this program: %s\n" (show program)
--           checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
--           case checkResult of
--             Nothing  -> return False
--             Just exs -> do
--               -- TODO add this back in
--               -- out <- toOutput env program exs
--               -- printResult $ encodeWithPrefix out
            
--               return True
--         else return False
    
--     -- Data.Bool.bool 

--     -- converts [a] to a Logic a
--     choices :: MonadPlus m => [a] -> m a
--     choices = msum . map return

-- {-
-- -- a -> a -> Int
-- -- Int -> Int -> Int    {a: Int}
-- --   [programs that return Int ]
-- --   none of these passed check
-- -- List -> List -> Int    {a: List}
-- --   [programs that return Int ]

-- -- observe 

-- -}
-- -- from paper: Data.Either.Right (Data.Tuple.snd ((,) arg0 arg1))
-- -- Don't have all args: 
-- --         Data.Either.Right arg1
-- --         Data.Either.Left (Data.Maybe.fromJust arg0)

-- -- stack run -- hplus --json='{"query": "Maybe a -> b -> Either a b", "inExamples": []}'
-- -- Data.Bool.bool (GHC.List.last []) (Data.Either.Right arg1) (Data.Maybe.isNothing arg0)
-- -- Computation time: 205.012 sec

-- -- head :: [a] -> a
-- -- [] :: [a]
 
-- -- head []     === undefined :: a

-- -- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- -- stack run -- hplus --json='{"query": "[(a,b)] -> a", "inExamples": []}'

-- -- Data.Tuple.fst (GHC.List.last arg0)
-- -- Data.Tuple.fst (GHC.List.head arg0)
-- -- Data.Tuple.fst (GHC.List.last arg0)
-- -- Data.Tuple.fst (GHC.List.head arg0)
-- -- Data.Tuple.fst (GHC.List.head arg0)
-- -- Data.Tuple.fst (GHC.List.last arg0)

-- -- == :: Eq a => a -> a -> Bool
-- --             A51  A52
-- -- 
-- --
-- -- does DFS stuff





-- -- example
-- --  we have: ( "f" . a51 -> [a51] -> Bool)
-- -- when we unify a51, we get [(id, schema, sub)]
-- -- can use sub to plug into the rest of the arguments
-- --  we should do the following
-- --    recurse on the first argument
-- --    for each function that unifies with the first argument (like type Int)
-- --      recurse on the second argument except the second argument is now [Int]
-- --    (backtrack until you find all the pairs of first and second arguments: like [a,b]
-- --    build things using f 
-- -- 






-- -- [a51, a51]
-- -- { a51: some type }
-- -- typeAssignment :: Map Id Sometypething
-- -- a :: SolverState
-- -- a = undefined 

-- --
-- dfs :: Environment -> Chan Message -> Int -> SType -> BTCompsSolver IO RProgram
-- dfs env messageChan depth goalType = lift $ do
  
--   -- collect all the component types (which we might use to fill the holes)
--   let components = Map.toList (env ^. symbols)


--   -- find all functions that unify with goal type
--   unifiedFuncs <- getUnifiedFunctions env messageChan components goalType :: BTCompsSolver IO [(Id, SType)]

--   -- for each of these functions, find solutions
--   functionSolutions <- mapM turnFunctionIntoSolutions unifiedFuncs :: BTCompsSolver IO [[RProgram]] -- [solutions for func1, solutions for func2]
--   let allFunctionSolutions = concat functionSolutions :: [RProgram]
--   return allFunctionSolutions
  
--   where
--     -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
--     isGround :: SType -> Bool
--     isGround (FunctionT id arg0 arg1) = False
--     isGround _ = True

--     turnFunctionIntoSolutions :: (Id, SType) -> CompsSolver IO [RProgram]
--     turnFunctionIntoSolutions (id, schema)
--       | isGround schema = return [Program { content = PSymbol id, typeOf = refineTop env schema }]
--       | depth == 0 = return []  -- stop if depth is 0
--       | otherwise = do

--         -- collect all the argument types (the holes ?? we need to fill)
--         -- get rid of any argument that has @@ (since they are for typeclasses, which we don't synthesize)
        
--         -- lift $ modify $ set typeAssignment Map.empty

--         -- let args = filter (not . \t -> "@@" `isInfixOf` show t) $ allArgTypes schema :: [SType]
--         let args = allArgTypes schema :: [SType]
  
--         -- recursively call DFS on the arguments' types to get the functions that return those types
--         let recurse = dfs env messageChan (depth-1) :: SType -> CompsSolver IO [RProgram]
--         solutionsPerArg <- mapM recurse args :: CompsSolver IO [[RProgram]] -- [[a,b,c], [d,e,f]]

--          -- [[a,[d,e]], [b,[d,e,f]], [c,[f]]]

--         -- get cartesian product of all the arguments
--         let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f]]

-- -- (GHC.List.sum :: @@hplusTC@@Num (Char) -> [Char] -> Char)

--         -- fill in arguments of func as RPrograms - [func a d, func a e, func a f, func b d, func b e, func b f]
--         let formatFn :: [RProgram] -> RProgram
--             formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
        
--         let finalResultList = map formatFn programsPerArg

--         -- when (id == "GHC.List.length") $ do
--         --   liftIO $ printf "trying to turn (%s :: %s) into programs that look like (%s ...args...) \n" id (show schema) id
--         --   liftIO $ printf "\t* args: %s\n" (show args)
--         --   liftIO $ printf "\t* programsPerArg: %s\n" (take 80 $ show programsPerArg)
--         --   liftIO $ printf "\t* solutionsPerArg: %s\n" (take 80 $ show solutionsPerArg)
--         --   liftIO $ printf "\t* finalResultList: %s\n" (take 80 $ show finalResultList)

--         return finalResultList

-- --
-- -- gets list of components/functions that unify with a given type
-- -- 
-- getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO [(Id, SType)]
-- getUnifiedFunctions envv messageChan xs goalType = do

--   modify $ set components []

--   st <- get
--   let memoized = st ^. memoize :: Map SType [(Id, SType)]

--   -- helper envv messageChan xs goalType
--   -- st <- get
--   -- return $ st ^. components
--   ------------------
--   case Map.lookup goalType memoized of
--     Just cs -> do
--       return cs
--     Nothing -> do
--       helper envv messageChan xs goalType
--       st <- get
--       let cs = st ^. components
--       modify $ set memoize (Map.insert goalType cs (st ^. memoize))
--       return $ st ^. components
  
--   where 
--     helper :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO ()
--     helper _ _ [] _ = return ()
    
--     helper envv messageChan ( v@(id, schema) : ys) goalType = do
--         (freshVars, st') <- lift $ do
          
--           freshVars <- freshType (envv ^. boundTypeVars) schema

--           -- liftIO $ print $ 

--           when (id == "==") $ do
--             liftIO $ printf "we used freshVars and turned schema %s into type %s\n" (show schema) (show freshVars)

--           let t1 = shape (lastType freshVars) :: SType
--           let t2 = goalType :: SType

--           modify $ set isChecked True
--           -- modify $ set typeAssignment Map.empty

--           solveTypeConstraint envv t1 t2 :: StateT CheckerState IO ()
--           st' <- get
          
--           return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

--         let sub =  st' ^. typeAssignment

--         -- a
--         -- int

--         -- map a::Int
--         -- schema: a -> a -> a
--         -- schema' : Int -> Int -> Int

--         let checkResult = st' ^. isChecked
--         -- liftIO $ putStrLn $ take 200 $ show sub
--         -- liftIO $ putStrLn $ show (id, "      ", t1, "      ", t2, "      ",freshVars,"      ",checkResult)

--         let schema' = stypeSubstitute sub (shape freshVars)

--         st <- get

--         -- if it unifies, add that particular unified compoenent to state's list of components
--         if (checkResult) 
--           then do
--             modify $ set components ((id, schema') : st ^. components) 
--           else return ()

--         helper envv messageChan ys goalType