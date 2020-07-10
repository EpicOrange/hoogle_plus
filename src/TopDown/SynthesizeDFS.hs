{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.SynthesizeDFS(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.TypeChecker
import HooglePlus.GHCChecker (check)
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Program
import Types.Solver
import Types.TopDown
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker
import PetriNet.Util

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.CPUTime
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


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
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- do
    --------------------------
    -- HIGHER ORDER STUFF 
    -- envWithHo <- if useHO -- add higher order query arguments
    --     then do
    --         let args = rawEnv ^. arguments
    --         let hoArgs = Map.filter (isFunctionType . toMonotype) args
    --         let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
    --         return $ rawEnv { 
    --             _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
    --             _hoCandidates = hoCands ++ map fst hoFuns
    --             }
    --     else do
    --------------------------

      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $ rawEnv {
          _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
          _hoCandidates = []
          }

    let depth = 3
    start <- getCPUTime

    printf "running bfs on %s\n" (show $ shape destinationType)
    
    -- used for figuring out which programs to filter (those without all arguments)
    let numArgs = length (Map.elems (envWithHo ^. arguments))

    let actualGoalType = shape destinationType :: SType
    solutions <- evalCompsSolver messageChan $ dfs envWithHo messageChan depth actualGoalType :: IO [RProgram]

    -- let startType = [Program { content = PHole, typeOf = refineTop envWithHo (shape destinationType) } ]
    
    -- bfs :: Environment -> Chan Message -> Int -> [RProgram] -> StateT CheckerState IO RProgram
    
    -- sol <- bfs envWithHo messageChan numArgs startType `evalStateT` emptyChecker { _checkerChan = messageChan } :: IO RProgram
    -- solution <- evalTopDownBackTrack messageChan $ do
    --   sol <- dfs envWithHo messageChan depth (shape destinationType) :: TopDownBackTrack IO RProgram
      
    --   guard (filterParams numArgs sol)
    --   -- guard (isInfixOf "arg1" (show sol))
      
    --   return sol
    
    -- print the first solution that has all the arguments
    -- mapM print $ take 1 $ filter (filterParams numArgs) solutions

    -- printf "done running dfsTop on %s\n" (show $ shape destinationType)
    -- printf "\n\n\nSOLUTION: %s\n\n\n" (show sol)




    -- print the first solution that has all the arguments
    -- mapM print $ take 1 $ filter (filterParams numArgs) solutions

    -- filter function for programs
    let check' :: RProgram -> IO Bool
        check' program = do
          -- printf "omg we are checking this program: %s\n" (show program)
          checkResult <- evalStateT (check envWithHo searchParams examples program goalType messageChan) emptyFilterState
          case checkResult of
            Nothing  -> return False
            Just exs -> do
              -- TODO add this back in
              -- out <- toOutput envWithHo program exs
              -- printResult $ encodeWithPrefix out
              return True

    -- let first10 = take 10 solutions
    -- mapM print $ take 1 $ solutions
    filtered <- filterM check' solutions :: IO [RProgram] -- let's hope it's lazy
    mapM print $ take 1 $ filtered
    
    end <- getCPUTime

    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)

-- ghcCheckedFunction = \thisArg -> sum (sum (last []) []) (take (sum thisArg) (last []))

-- sum :: Num a => [a] -> a
-- last :: [a] -> a

-- GHC.List.sum (GHC.List.last ([] :: [[a]]) ) []
-- ------------
    
    writeChan messageChan (MesgClose CSNormal)
    return ()

    where
      
      -- determines if the result has all the appropriate arguments given the number of args
      filterParams :: Int -> RProgram -> Bool
      filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
      filterParams 1       x = "arg0" `isInfixOf` (show x)
      filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x


type CompsSolver m = StateT Comps (StateT CheckerState m)
evalCompsSolver messageChan m = m `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- type TopDownBackTrack m = LogicT (StateT CheckerState m)
-- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- -- evalTopDownBackTrack messageChan action = observeAllT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
-- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- -- evalTopDownBackTrack messageChan action = fmap (\x -> [x]) $ observeT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
-- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m a
-- evalTopDownBackTrack messageChan action = observeT action `evalStateT` (emptyChecker { _checkerChan = messageChan })


instance Monad m => CheckMonad (CompsSolver m) where
    getNameCounter = lift getNameCounter
    setNameCounter = lift . setNameCounter
    getNameMapping = lift getNameMapping
    setNameMapping = lift . setNameMapping
    getIsChecked   = lift getIsChecked
    setIsChecked   = lift . setIsChecked
    getMessageChan = lift getMessageChan
    overStats      = lift . overStats

--
-- does DFS stuff
--
dfs :: Environment -> Chan Message -> Int -> SType -> CompsSolver IO [RProgram]
dfs env messageChan depth goalType = do
  
  -- collect all the component types (which we might use to fill the holes)
  let components = Map.toList (env ^. symbols)

  -- find all functions that unify with goal type
  unifiedFuncs <- getUnifiedFunctions env messageChan components goalType :: CompsSolver IO [(Id, SType)]

  -- for each of these functions, find solutions
  functionSolutions <- mapM turnFunctionIntoSolutions unifiedFuncs :: CompsSolver IO [[RProgram]] -- [solutions for func1, solutions for func2]
  let allFunctionSolutions = concat functionSolutions :: [RProgram]
  return allFunctionSolutions
  
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True

    turnFunctionIntoSolutions :: (Id, SType) -> CompsSolver IO [RProgram]
    turnFunctionIntoSolutions (id, schema)
      | isGround schema = return [Program { content = PSymbol id, typeOf = refineTop env schema }]
      | depth == 0 = return []  -- stop if depth is 0
      | otherwise = do

        -- collect all the argument types (the holes ?? we need to fill)
        -- get rid of any argument that has @@ (since they are for typeclasses, which we don't synthesize)
        
        -- let args = filter (not . \t -> "@@" `isInfixOf` show t) $ allArgTypes schema :: [SType]
        let args = allArgTypes schema :: [SType]
  
        -- recursively call DFS on the arguments' types to get the functions that return those types
        let recurse = dfs env messageChan (depth-1) :: SType -> CompsSolver IO [RProgram]
        solutionsPerArg <- mapM recurse args :: CompsSolver IO [[RProgram]] -- [[a,b,c], [d,e,f]]

        -- get cartesian product of all the arguments
        let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f]]

-- (GHC.List.sum :: @@hplusTC@@Num (Char) -> [Char] -> Char)

        -- fill in arguments of func as RPrograms - [func a d, func a e, func a f, func b d, func b e, func b f]
        let formatFn :: [RProgram] -> RProgram
            formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
        
        let finalResultList = map formatFn programsPerArg

        -- when (id == "GHC.List.length") $ do
        --   liftIO $ printf "trying to turn (%s :: %s) into programs that look like (%s ...args...) \n" id (show schema) id
        --   liftIO $ printf "\t* args: %s\n" (show args)
        --   liftIO $ printf "\t* programsPerArg: %s\n" (take 80 $ show programsPerArg)
        --   liftIO $ printf "\t* solutionsPerArg: %s\n" (take 80 $ show solutionsPerArg)
        --   liftIO $ printf "\t* finalResultList: %s\n" (take 80 $ show finalResultList)

        return finalResultList

--
-- gets list of components/functions that unify with a given type
-- 
getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO [(Id, SType)]
getUnifiedFunctions envv messageChan xs goalType = do

  modify $ set components []

  st <- get
  let memoized = st ^. memoize :: Map SType [(Id, SType)]

  -- helper envv messageChan xs goalType
  -- st <- get
  -- return $ st ^. components
  ------------------
  case Map.lookup goalType memoized of
    Just cs -> do
      return cs
    Nothing -> do
      helper envv messageChan xs goalType
      st <- get
      let cs = st ^. components
      modify $ set memoize (Map.insert goalType cs (st ^. memoize))
      return $ st ^. components
  
  where 
    helper :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO ()
    helper _ _ [] _ = return ()
    
    helper envv messageChan ( v@(id, schema) : ys) goalType = do
        (freshVars, st') <- lift $ do

          freshVars <- freshType (envv ^. boundTypeVars) schema

          let t1 = shape (lastType freshVars) :: SType
          let t2 = goalType :: SType

          modify $ set isChecked True
          modify $ set typeAssignment Map.empty

          solveTypeConstraint envv t1 t2 :: StateT CheckerState IO ()
          st' <- get
          
          return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

        let sub =  st' ^. typeAssignment
        let checkResult = st' ^. isChecked
        -- liftIO $ putStrLn $ show (id, "      ", t1, "      ", t2, "      ",freshVars,"      ",checkResult)

        let schema' = stypeSubstitute sub (shape freshVars)

        st <- get

        -- if it unifies, add that particular unified compoenent to state's list of components
        if (checkResult) 
          then do
            modify $ set components ((id, schema') : st ^. components) 
          else return ()

        helper envv messageChan ys goalType
