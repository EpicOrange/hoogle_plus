{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BottomUp.Synthesize(synthesize, envToGoal) where

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
    let destinationType = lastType (toMonotype goalType) :: RType
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- do
    
     --------------------------
      -- HIGHER ORDER STUFF 
      -- let args = rawEnv ^. arguments
      -- let hoArgs = Map.filter (isFunctionType . toMonotype) args
      -- let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
      -- return $ rawEnv { 
      --     _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
      --     _hoCandidates = hoCands ++ map fst hoFuns
      --     }
    --------------------------

      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $ rawEnv {
          _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
          _hoCandidates = []
          }

    -- call dfs with iterativeDeepening
    -- iterativeDeepening envWithHo messageChan searchParams examples goalType
    printf "===============\nStarting....\n===============\n"

    program <- evalBottomUpSolver messageChan $ do
      program' <- dothing messageChan envWithHo (shape destinationType)
      checkResult <- liftIO $ check' program' envWithHo goalType
      guard checkResult
      return program'
    
    
    printf "===============\n\nRESULT: %s \n\n===============\n" (show program)
    writeChan messageChan (MesgClose CSNormal)
    return ()
    
    where
      -- wrapper for `check` function
      check' :: RProgram -> Environment -> RSchema -> IO Bool
      check' program env goalType
        -- check if the program has all the arguments that it should have (avoids calling check)
        | filterParams program env = do
            liftIO $ printf "calling check on program: %s\n" $ show program
            checkResult <- evalStateT (check env searchParams examples program goalType messageChan) emptyFilterState
            case checkResult of
              Nothing  -> return False
              Just exs -> do
                out <- toOutput env program exs
                printResult $ encodeWithPrefix out
                return True
        | otherwise = return False
        
      -- determines if the result has all the appropriate arguments
      filterParams :: RProgram -> Environment -> Bool
      filterParams program env = all (`isInfixOf` (show program)) $ Map.keys $ env ^. arguments


type BottomUpSolver m = StateT CheckerState (LogicT m)

evalBottomUpSolver :: Monad m => Chan Message -> BottomUpSolver m a -> m a
evalBottomUpSolver messageChan m = do
  observeT $
    (`evalStateT` emptyChecker {_checkerChan = messageChan}) $ m

evalBottomUpSolverList :: Monad m => Chan Message -> BottomUpSolver m a -> m [a]
evalBottomUpSolverList messageChan m = do
    observeManyT 20 $
      (`evalStateT` emptyChecker {_checkerChan = messageChan}) $ m


-- TODO this
dothing :: Chan Message -> Environment -> SType -> BottomUpSolver IO RProgram
dothing messageChan env goalType = do

  -- collect all the component types (which we might use to fill the holes)
  -- component <- choices $ Map.toList (env ^. symbols)
  let components = Map.toList (env ^. symbols)
  -- _symbols :: Map Id RSchema,          -- ^ Variables and constants (with their refinement types), indexed by arity

  -- populates P with all the ground types from env
  -- and turns them into functions
  let buildP :: (Id, RSchema) -> [RProgram] -> [RProgram]
      buildP (id, schema) acc
        | schema' <- shape $ toMonotype schema,
          isGround schema' = Program { content = PSymbol id, typeOf = refineTop env schema' } : acc
        | otherwise       = acc
  
  let myP = foldr buildP [] (components) :: [RProgram]

  -- liftIO $ printf "myComponents: %s\n" (show myComponents)

  liftIO $ putStrLn $ "we are here omg"
  let growWrapper :: BottomUpSolver IO RProgram -> Int -> BottomUpSolver IO RProgram
      -- growWrapper pSt 5 = pSt
      growWrapper pSt depth = do  
        
        -- pair@(pSt, comptype) <- pSt
        liftIO $ printf "depths %d... \n" depth
  
        -- liftIO $ printf "component: %s \n" (show component)

        pStList <- liftIO $ evalBottomUpSolverList messageChan pSt :: BottomUpSolver IO [RProgram]
        liftIO $ printf "pSt: %s\n" (show pStList)

        -- growWRapper assigns comptype
        -- grow returns rprograms, which are all 'new'
        let pSt' = grow components pSt


        pSt'List <- liftIO $ evalBottomUpSolverList messageChan pSt' :: BottomUpSolver IO [RProgram]
        liftIO $ printf "pSt': %s\n" (show pSt'List)

        
        -- first iteration: growWrapper [ a,b,c,d ]
        -- pSt is [a, b, c, d] 
        -- pSt' is [f c, f d, g a, g b] 
        -- returns [a, b, c, d] `mplus` growWrapper [ a,b,c,d,f c,f d,g a,g b ]

        -- second iteration: growWrapper [ a,b,c,d,f c,f d,g a,g b ]
        -- pSt is [ a,b,c,d,f c,f d,g a,g b ]
        -- pSt' is [f c, f d, g a, g b, g (f c) ... ] 
        -- returns [ a,b,c,d,f c,f d,g a,g b ] `mplus` growWrapper [ a,b,c,d,f c,f d,g a,g b] `mplus` [f c, f d, g a, g b, g (f c) ... ] 

        -- [a, b, c, d] `mplus`  .....
        -- [a, b, c, d] `mplus`  ([a, b, c, d, f c, f d, g a, g b ] `mplus` ...)

        -- idea: add old to pst
              -- add new to pst'
        pSt `mplus` growWrapper (pSt `mplus` pSt') (depth + 1)


        -- pSt `mplus` growWrapper pSt' (depth + 1)

        -- grow [new] [old]
        -- grow [(f a, new), (a, old)]
        -- guard (is there at least one new???)

        -- grow [ f c,f d,g a,g b ] [ a,b,c,d, ]
        -- somehow force grow to use at least one program from the last depth, to synthesize the next depth

  grownProgram <- growWrapper (choices myP) 1 :: BottomUpSolver IO RProgram

  
  -- want to add grownProgram to myP 
  -- how do we do this if we need to backtrack to get the next grownProgram????????????
  guard =<< isUnifiedComp grownProgram goalType
  
  liftIO $ printf "found program: %s \n" (show grownProgram)
  
  return grownProgram
  
  where
    myInt        = ScalarT (DatatypeT "Int" [] []) ftrue :: RType
    myBool       = ScalarT (DatatypeT "Bool" [] []) ftrue :: RType
    myList a     = ScalarT (DatatypeT "List" [a] []) ftrue :: RType
    myIntSchema  = Monotype myInt :: RSchema
    myBoolSchema = Monotype myBool :: RSchema
    myFn a b     = FunctionT "" a b
    -- myTVar = refineTop
    fType = Monotype $ myFn myBool myInt :: RSchema
    gType = Monotype $ myFn myInt (myList myInt) :: RSchema
    hType = Monotype $ myFn myInt (myFn myBool (myFn (myList myInt) myInt)) :: RSchema
    myComponents = Map.fromList [("a", myIntSchema),("b", myIntSchema),("c", myBoolSchema),("d", myBoolSchema),
                        ("f", fType),("g", gType),("h", hType)] :: Map Id RSchema
    -- let components = Map.toList myComponents :: [(Id, RSchema)]
    -- temp env with fewer components
    env' = set symbols myComponents env
    
    -- fill in args of schema using the ground programs we already have
    -- (instead of calling dfs on each argument)
    -- g ?? ?? ??    <- fill these holes :: SType
    --    using programs in P
    --    returning a program
    -- comp represents 1 component from the environment 
    grow :: [(Id, RSchema)] -> BottomUpSolver IO RProgram -> BottomUpSolver IO RProgram
    grow    components         programs = do

      component@(id, schema) <- choices components
  
      -- when (isInfixOf "ength" id ) $ do
      --   liftIO $ printf "length exists i guess\n"

      -- when (id == "h") $ do

      --   liftIO $ 
      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema :: BottomUpSolver IO RType
      let schema' = shape freshVars

      -- stream of solutions to the (id, schema) returned from getUnifiedComponent

      -- TODO filter these ones out at the beginning so that they are not repeated
      guard (not $ isGround schema')
      
      -- liftIO $ printf "Growing.... compId: %s \n" (id)

      -- program <- choices myP :: BottomUpSolver IO RProgram
      
      -- let programType = typeOf program :: RType
      -- let retType = shape $ lastType programType :: SType
      
      -- collect all the argument types (the holes ?? we need to fill)
      let args = allArgTypes schema' :: [SType]

      let func1 :: SType -> BottomUpSolver IO RProgram
          func1 argType = do

            program <- programs
            isUnified <- isUnifiedComp program argType
            -- when (isInfixOf "ength" id ) $ do
        -- liftIO $ printf "length exists i guess\n"
              -- liftIO $ printf "component: %s, argType: %s, program: %s, isUnified: %s \n" (show component) (show argType) (show program) (show isUnified)
            
            guard isUnified
            return program
      -- TODO change to matching everything from p that it unifies with
      -- let func = getUnifiedComponent myP :: SType -> BottomUpSolver IO [RProgram]
      argsFilled <- mapM func1 args :: BottomUpSolver IO [RProgram]

      -- when (isInfixOf "ength" id ) $ do
      --   liftIO $ printf "component: %s, argsFilled: %s \n" (show component) (show argsFilled)
            

      let programNew = Program { content = PApp id argsFilled, typeOf = refineTop env schema' } 
      return programNew
        
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- return whether or not the program type unifies with the return type of the goal
    isUnifiedComp :: RProgram -> SType -> BottomUpSolver IO Bool
    isUnifiedComp program goalType = do
                   
                   
      let programType = shape $ typeOf program :: SType

      -- -- replaces "a" with "tau1"
      -- freshVars <- freshType (env ^. boundTypeVars) schema

      -- let t1 = shape (lastType freshVars) :: SType
      let t1 = programType :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: BottomUpSolver IO ()
      st' <- get
      
      -- TODO what can we do with sub? 
      -- let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      -- liftIO $ printf "program: %s, programType: %s, goalType: %s, checkResult: %s \n" (show program) (show programType) (show goalType) (show checkResult)
      -- guard checkResult
      -- return (id, stypeSubstitute sub (shape freshVars))
      return checkResult


    -- -- get every program from P that unifies with goal
    -- getUnifiedComponent :: RProgram -> SType -> BottomUpSolver IO RProgram
    -- getUnifiedComponent program goalType = do
                          
    --   let programType = typeOf program :: RType
    --   let retType = shape $ lastType programType :: SType

    --   -- -- replaces "a" with "tau1"
    --   -- freshVars <- freshType (env ^. boundTypeVars) schema

    --   -- let t1 = shape (lastType freshVars) :: SType
    --   let t1 = retType :: SType
    --   let t2 = goalType :: SType

    --   solveTypeConstraint env t1 t2 :: BottomUpSolver IO ()
    --   st' <- get
      
    --   -- TODO what can we do with sub? 
    --   -- let sub = st' ^. typeAssignment
    --   let checkResult = st' ^. isChecked
      
    --   guard checkResult
    --   -- return (id, stypeSubstitute sub (shape freshVars))
    --   return program









---------------------------
---------------------------
--------- OLD CODE --------
---------------------------
---------------------------






-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
-- iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
-- iterativeDeepening env messageChan searchParams examples goal = evalBottomUpSolver messageChan $ do
--   -- liftIO $ printf "running dfs on %s at size %d\n" (show goal) quota

--   let goalType = shape $ lastType (toMonotype goal) :: SType
--   solution <- dfs env messageChan quota goalType :: BottomUpSolver IO RProgram
  
--   -- liftIO $ printf "solution: %s\n" (show solution)
--   isChecked <- liftIO $ check' solution
--   guard isChecked -- gets the first valid program

--   return ()
-- -- iterativeDeepening env messageChan searchParams examples goal = evalBottomUpSolverList messageChan (map helper [5..]) >> return ()
--   where
--     -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
--     -- in filterParams
--     filterOutTypeClass :: [Id] -> [Id]
--     filterOutTypeClass xs = filter (not . \x -> "tc" `isPrefixOf` (show x)) xs
  
--     -- calls dfs at a certain depth and checks to see if there is a solution
--     helper :: BottomUpSolver IO RProgram
--     helper =BottomUp    -- wrapper for `check` function
--     check' :: RProgram -> IO Bool
--     check' program
--       -- check if the program has all the arguments that it should have (avoids calling check)
--       | filterParams program = do
--           -- liftIO $ printf "program: %s\n" $ show program
--           checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
--           case checkResult of
--             Nothing  -> return False
--             Just exs -> do
--               out <- toOutput env program exs
--               printResult $ encodeWithPrefix out
--               return True
--       | otherwise = return False

--     -- determines if the result has all the appropriate arguments
--     filterParams :: RProgram -> Bool
--     filterParams program = all (`isInfixOf` (show program)) $ Map.keys $ env ^. arguments

--
-- does DFS stuff, with max size of program given as a quota
--
-- dfs :: Environment -> Chan Message -> Int -> SType -> BottomUpSolver IO RProgram
-- dfs env messageChan quota goalType
--   | quota <= 0 = mzero
--   | otherwise  = do

--     -- collect all the component types (which we might use to fill the holes)
--     component <- choices $ Map.toList (env ^. symbols)
    
--     -- stream of components that unify with goal type
--     (id, schema) <- getUnifiedComponent component :: BottomUpSolver IO (Id, SType)
    
--     -- stream of solutions to the (id, schema) returned from getUnifiedComponent
--     if isGround schema
--       then return Program { content = PSymbol id, typeOf = refineTop env schema }
--       else do
--         -- collect all the argument types (the holes ?? we need to fill)
--         let args = allArgTypes schema :: [SType]

--         -- do basically this:
--         -- dfsstuff0 <- dfs ... arg0 (quota - 1) :: RProgram
--         -- dfsstuff1 <- dfs ... arg1 (quota - 1 - sizeOf dfsstuff0) :: RProgram
--         -- dfsstuff2 <- dfs ... arg2 (quota - 1 - sizeOf dfsstuff0 - sizeOf dfsstuff1) :: RProgram
--         -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
--         let func :: (Int, [RProgram]) -> SType -> BottomUpSolver IO (Int, [RProgram])
--             func (quota', programs) arg = do
--               program <- dfs env messageChan quota' arg
--               return (quota' - sizeOf program, programs ++ [program])

--         (_, argsFilled) <- foldM func (quota - 1, []) args :: BottomUpSolver IO (Int, [RProgram])

--         return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 
      
--   where
--     -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
--     isGround :: SType -> Bool
--     isGround (FunctionT id arg0 arg1) = False
--     isGround _ = True
    
--     -- converts [a] to a Logic a
--     choices :: MonadPlus m => [a] -> m a
--     choices = msum . map return

--     -- gets the size of a program, used for checking quota
--     sizeOf :: RProgram -> Int
--     sizeOf = length . words . show

--     -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
--     --  returns updated schema w/ sub if unifies 
--     getUnifiedComponent :: (Id, RSchema) -> BottomUpSolver IO (Id, SType)
--     getUnifiedComponent (id, schema) = do
      
--       -- replaces "a" with "tau1"
--       freshVars <- freshType (env ^. boundTypeVars) schema

--       let t1 = shape (lastType freshVars) :: SType
--       let t2 = goalType :: SType

--       solveTypeConstraint env t1 t2 :: BottomUpSolver IO ()
--       st' <- get
      
--       let sub = st' ^. typeAssignment
--       let checkResult = st' ^. isChecked
      
--       guard checkResult
--       return (id, stypeSubstitute sub (shape freshVars))
