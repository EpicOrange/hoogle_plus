module HooglePlus.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import Synquid.Error
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type
import Types.Abstract
import HyperGraph.HyperExplorer

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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


updateEnvWithBoundTyVars :: SchemaSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: TypeSkeleton -> Environment -> (Environment, TypeSkeleton)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = 
  updateEnvWithSpecArgs tRes $ addVariable x tArg $ addArgument x tArg env
updateEnvWithSpecArgs ty env = (env, ty)

envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> putDoc (pretty $ toErrorMessage parseErr) >> putDoc empty >> error "uh oh"
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> do
            print sp
            let (env', monospec) = updateEnvWithBoundTyVars sp env
            let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
            return $ goal { gEnvironment = env'', gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc empty >> exitFailure

      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> IO ()
synthesize searchParams goal = do
    -- test hypergraph
    let g = Map.fromList [ (Set.fromList [ATypeVarT "s" KnStar], [ (Set.fromList [ATypeVarT "s" KnStar], "f1", Set.fromList [ATypeVarT "1" KnStar]), (Set.fromList [ATypeVarT "s" KnStar], "f2", Set.fromList [ATypeVarT "2" KnStar]), (Set.fromList [ATypeVarT "s" KnStar], "", Set.fromList [ATypeVarT "2" KnStar, ATypeVarT "c" KnStar]) ])
            , (Set.fromList [ATypeVarT "1" KnStar, ATypeVarT "2" KnStar], [ (Set.fromList [ATypeVarT "1" KnStar, ATypeVarT "2" KnStar], "", Set.fromList [ATypeVarT "3" KnStar]) ])
            , (Set.fromList [ATypeVarT "2" KnStar], [ (Set.fromList [ATypeVarT "2" KnStar], "", Set.fromList [ATypeVarT "3" KnStar, ATypeVarT "4" KnStar]) ])
            , (Set.fromList [ATypeVarT "3" KnStar], [ (Set.fromList [ATypeVarT "3" KnStar], "", Set.fromList [ATypeVarT "4" KnStar]) ])
            , (Set.fromList [ATypeVarT "3" KnStar, ATypeVarT "4" KnStar], [ (Set.fromList [ATypeVarT "3" KnStar, ATypeVarT "4" KnStar], "", Set.fromList [ATypeVarT "t" KnStar]) ])
            , (Set.fromList [ATypeVarT "4" KnStar], [ (Set.fromList [ATypeVarT "4" KnStar], "", Set.fromList [ATypeVarT "t" KnStar]) ])
            , (Set.fromList [ATypeVarT "c" KnStar], [ (Set.fromList [ATypeVarT "c" KnStar], "", Set.fromList [ATypeVarT "4" KnStar, ATypeVarT "d" KnStar])])
            , (Set.fromList [ATypeVarT "a" KnStar, ATypeVarT "1" KnStar], [ (Set.fromList [ATypeVarT "a" KnStar, ATypeVarT "1" KnStar], "", Set.fromList [ATypeVarT "b" KnStar])])
            , (Set.fromList [ATypeVarT "b" KnStar], [ (Set.fromList [ATypeVarT "b" KnStar], "", Set.fromList [ATypeVarT "t" KnStar]), (Set.fromList [ATypeVarT "b" KnStar], "", Set.fromList [ATypeVarT "a" KnStar])]) ]
    paths <- observeManyT (_solutionCnt searchParams) (getBFPath (ATypeVarT "s" KnStar) (ATypeVarT "t" KnStar) g)
    print paths
    error "stop"
    let env' = gEnvironment goal
    let destinationType = lastType $ toMonotype $ gSpec goal
    let useHO = _useHO searchParams
    let rawSyms = env' ^. symbols
    let hoCands = env' ^. hoCandidates
    env <-
        if useHO -- add higher order query arguments
            then do
                let args = env' ^. arguments
                let hoArgs = Map.filter (isFunctionType . toMonotype) args
                let hoFuns = map (\(k, v) -> (k ++ hoPostfix, toFunType v)) (Map.toList hoArgs)
                return $
                    env'
                        { _symbols = rawSyms `Map.union` Map.fromList hoFuns
                        , _hoCandidates = hoCands ++ map fst hoFuns
                        }
            else do
                let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
                return $
                    env'
                        {_symbols = Map.withoutKeys syms $ Set.fromList hoCands, _hoCandidates = []}
    -- putStrLn $ "Component number: " ++ show (Map.size $ allSymbols env)
    let args = Monotype destinationType : Map.elems (env ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
    let rs = _refineStrategy searchParams
    let is =
            emptySolverState
                { _searchParams = searchParams
                , _abstractionCover =
                      case rs of
                          SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
                          TyGar0 -> emptySolverState ^. abstractionCover
                          TyGarQ -> Abstraction.specificAbstractionFromTypes env args
                          NoGar -> Abstraction.specificAbstractionFromTypes env args
                          NoGar0 -> emptySolverState ^. abstractionCover
                -- , _messageChan = messageChan
                }
    evalStateT (runPNSolver env destinationType) is
    -- catch
    --     (evalStateT (runPNSolver env destinationType) is)
    --     (\e ->
    --          writeChan messageChan (MesgLog 0 "error" (show e)) >>
    --          writeChan messageChan (MesgClose (CSError e)) >>
    --          error (show e))
    return ()
