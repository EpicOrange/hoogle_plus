{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.Synthesize(synthesize, envToGoal) where

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

-- -- | Synthesis goal
-- data Goal = Goal {
--   gName :: Id,                  -- ^ Function name
--   gEnvironment :: Environment,  -- ^ Enclosing environment
--   gSpec :: RSchema,             -- ^ Specification
--   gImpl :: UProgram,            -- ^ Implementation template
--   gDepth :: Int,                -- ^ Maximum level of auxiliary goal nesting allowed inside this goal
--   gSourcePos :: SourcePos       -- ^ Source Position
-- } deriving (Eq, Ord)

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
    -- printf "rawEnv: %s\n" (show $ rawEnv ^. symbols)
    let goalType = gSpec goal :: RSchema
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- do
    
    --------------------------
      -- HIGHER ORDER STUFF 
      let args = rawEnv ^. arguments
      -- let hoArgs = Map.filter (isFunctionType . toMonotype) args
      -- let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
      return $ rawEnv { 
          _symbols = rawSyms, -- `Map.union` Map.fromList hoFuns, 
          _hoCandidates = hoCands -- ++ map fst hoFuns
          }
    --------------------------
      -- let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      -- return $ rawEnv {
      --     _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
      --     _hoCandidates = []
      --     }
    --------------------------

    -- TODO this list still includes some 'ho stuff with Fun types, get rid of them!
    -- ("GHC.List.head",<a> . ([a] -> a))
    -- ("GHC.List.head_0'ho'",<a> . Fun (([a])) (a))
    -- ("Data.Function.id",<a> . (a -> a))
    -- ("Data.Function.id_0'ho'",<a> . Fun (a) (a))
    -- ("Data.Either.Left",<b> . <a> . (a -> Either (a) (b)))
    -- ("Data.Either.Left_0'ho'",<b> . <a> . Fun (a) ((Either (a) (b))))
    -- ("Data.Either.Right",<b> . <a> . (b -> Either (a) (b)))
    -- ("Data.Either.Right_0'ho'",<b> . <a> . Fun (b) ((Either (a) (b))))
    -- ("(Data.Eq./=)",<a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool))))
    -- ("(Data.Eq./=)_0'ho'",<a> . (@@hplusTC@@Eq (a) -> (a -> Fun (a) (Bool))))
    -- ("(Data.Eq./=)_1'ho'",<a> . (@@hplusTC@@Eq (a) -> Fun (a) ((Fun (a) (Bool)))))
    -- ("(Data.Eq./=)_2'ho'",<a> . Fun ((@@hplusTC@@Eq (a))) ((Fun (a) ((Fun (a) (Bool))))))
    mapM print $ Map.toList $ envWithHo ^. symbols

    putStrLn "\n=================="
    putStrLn "Starting!"
    print $ Map.keys $ envWithHo ^. arguments
    putStrLn "=================="

    -- call dfs with iterativeDeepening
    -- iterativeDeepening envWithHo messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    return ()


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m = do
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan (map helper [1..20]) >> return ()
-- iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan (map helper [5..]) >> return ()
  where
    -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
    -- in filterParams
    filterOutTypeClass :: [Id] -> [Id]
    filterOutTypeClass xs = filter (not . \x -> "tc" `isPrefixOf` (show x)) xs

  
    -- calls dfs at a certain depth and checks to see if there is a solution
    helper :: Int -> TopDownSolver IO RProgram
    helper quota = do
      liftIO $ printf "running dfs on %s at size %d\n" (show goal) quota

      let goalType = shape $ lastType (toMonotype goal) :: SType
      solution <- dfs env messageChan quota goalType :: TopDownSolver IO RProgram
      
      -- guard (isInfixOf "filter" (show solution))
      liftIO $ printf "solution: %s\n" (show solution)
      isChecked <- liftIO $ check' solution
      guard isChecked -- gets the first valid program
      return solution
    -- fix :: (a->a) -> a
    --        SType   a1 -> a2
    -- arg1 :: a -> Bool
    -- goal  is   Int
    -- wrapper for `check` function
    check' :: RProgram -> IO Bool
    check' program
      -- check if the program has all the arguments that it should have (avoids calling check)
      | filterParams program env = do
          -- liftIO $ printf "program: %s\n" $ show program
          checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
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

--
-- does DFS stuff, with max size of program given as a quota
--
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownSolver IO RProgram
dfs env messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = do

    -- collect all the component types (which we might use to fill the holes)
    component <- choices $ Map.toList (env ^. symbols)

    -- stream of components that unify with goal type
    (id, schema) <- getUnifiedComponent component :: TopDownSolver IO (Id, SType)
    
    liftIO $ printf "goalType: %s, id: %s (%s)\n" (show goalType) id (show schema) 
    -- stream of solutions to the (id, schema) returned from getUnifiedComponent
    if isGround schema
      then return Program { content = PSymbol id, typeOf = refineTop env schema }
      else do
        -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]

        -- liftIO $ printf "goalType: %s, id: %s (%s)\n" (show goalType) id (show schema) 

        -- do basically this:
        -- dfsstuff0 <- dfs ... arg0 (quota - 1) :: RProgram
        -- dfsstuff1 <- dfs ... arg1 (quota - 1 - sizeOf dfsstuff0) :: RProgram
        -- dfsstuff2 <- dfs ... arg2 (quota - 1 - sizeOf dfsstuff0 - sizeOf dfsstuff1) :: RProgram
        -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
        let func :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
            func (quota', programs) arg = do
              -- liftIO $ printf "\t* arg: %s\n" (show arg)
              if (isFunctionType arg) -- arg is e.g. a -> Bool
                then do
                  (newEnv, newQuery) <- do
                    let monospec = refineTop env arg :: RType
                    -- when (id == "Data.Function.fix") $ liftIO $ printf "id: %s (%s), arg: %s, monospec: %s\n" id (show schema) (show $ arg) (show $ monospec)
                    -- liftIO $ printf "\t*** arg: %s, monospec: %s\n" (show schema) (show $ arg) (show $ monospec)

                    -- let (env', monospec::RType) = updateEnvWithBoundTyVars polyspecquery env
                    let (env', destinationType) = updateEnvWithSpecArgs monospec env
                    -- difference :: Ord k => Map k a -> Map k b -> Map k a
                    -- let newArgs = Map.difference (env' ^. arguments) (env ^. arguments)
                    
                    -- liftIO $ printf "symbols: %s\n" (show $ env' ^. symbols)
                    -- liftIO $ printf "id: %s, arguments: %s\n" id (show $ env' ^. arguments)
                    -- -- liftIO $ printf "newArgs: %s\n" (show newArgs)
                    -- liftIO $ printf "destinationType: %s\n" (show destinationType)
                    -- liftIO $ printf "arg: %s\n" (show arg)

                    -- liftIO $ print $ env' ^. arguments
                    return $ (env', destinationType)
                  body <- dfs newEnv messageChan quota' (shape newQuery)
                  let newArgs = Map.toList (Map.difference (newEnv ^. arguments) (env ^. arguments)) :: [(Id, RSchema)]


                  -- try to eta reduce:
                  -- if body looks like ((f b) c) d
                  --  = PApp "f" [PSymbol "b", PSymbol "c", PSymbol "d"]
                  -- and newArgs looks like [a,c,d]
                  -- change them to look like
                  --   body: f b
                  --   newArgs: [a]

                  -- daniel's attempt????
                  -- let etareduce (Program (PApp id xs) t, args)
                  --       | null xs              -> (PSymbol id, args)
                  --       | last xs == last args -> etareduce (Program (PApp Id (init xs)) t) (init args)
                  --     etareduce (prog, args) = (prog, args)
                  -- let (body', newArgs') = etareduce (body, newArgs)
                        


                  -- liftIO $ printf "id %s, program: %s\n" id (show program)
                  -- todo: add in the arguments to program
                  -- return undefined
                  let foo (id',t') body'@(Program _ bodyType) =
                        Program { 
                          content = PFun id' body', 
                          typeOf = FunctionT id' (toMonotype t') (bodyType)
                        }
                  let program = foldr foo body newArgs
                  -- TODO eta reduce this result

                  -- return (quota' - sizeOf body, programs ++ [program])
                  return (quota' - sizeOf program, programs ++ [program])
                  -- call dfs with newEnv
                else do
                  program <- dfs env messageChan quota' arg
                  return (quota' - sizeOf program, programs ++ [program])

        (_, argsFilled) <- foldM func (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])

        return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 

  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- gets the size of a program, used for checking quota
    sizeOf :: RProgram -> Int
    sizeOf = length . words . show

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    --  returns updated schema w/ sub if unifies 
    getUnifiedComponent :: (Id, RSchema) -> TopDownSolver IO (Id, SType)
    getUnifiedComponent (id, schema) = do
          
      -- when (id == "Data.List.map") $ liftIO $ printf "id: %s (%s)\n" id (show schema) 
      -- guard ( id == "GHC.List.map" || "Pair" `isInfixOf` id || "arg" `isPrefixOf` id )
      -- liftIO $ putStrLn $ "matched! " ++ id
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
