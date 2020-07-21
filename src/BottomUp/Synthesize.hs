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
    let destinationType = lastType (toMonotype goalType)
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

    writeChan messageChan (MesgClose CSNormal)
    return ()


{-

bottom up example
-----------------

Query: Int -> Int

Ground programs: arg0 :: Int
Other components: 
    length :: [a]  -> Int
    f      :: Bool -> Int
    g      :: Int  -> [Int]
    !!     :: [a]  -> Int -> a

Depth 1 ground programs: (arg0 :: Int)
Depth 2 ground programs: (g arg0 :: [Int])
Depth 3 ground programs: (length (g arg0) :: Int)    (!! [a] arg0 :: a)

a :: Int
b :: Int
c :: Bool 
d :: Bool

g :: Int -> Bool -> [Int] -> Int

grow (func = g) P = do
  -- g a c
  -- g b d

  arg0 <- matches int  -- stream of components that match with Int from P 
  arg1 <- matches bool -- stream of components that match with Int from p
  arg2 <- matches [Int]

  -- build up program
  return program -- e.g.   g a c

loop :: [RProgram] ->     -- P
        [RProgram] ->     -- P
        [RProgram]

     P not_returned_yet       P' already_returned 
loop (P = stream [a,b,c,d])  (P' = [])            = do

  func <- components from env -- stream of [f,g,h]
  let P'' = grow func (P mplus P')        -- stream of [g a c, g a d, g b c, g b d]
  
  return $ P            = stream [a,b,c,d]) 
           `mplus`
          loop P''       =  grow func (P mplus P') = 
              (P mplus P')   -- try to produce a stream [g a c, g a d, g b c, g b d, ] `mplus` [a,b,c,d] 
  
  
--- can't really separate returned and not returned programs
--- what if we use a set? but it might not work well with streams


     P not_returned_yet                          P' already_returned 
loop (P = stream [g a c, g a d, g b c, g b d])  (P' = [a,b,c,d])            = do

  func <- components from env -- stream of [f,g,h]
  let P'' = grow func (P mplus P')        -- stream of [g a c, g a d, g b c, g b d]
  

P'' = [g (g a c) c, g (g a d) c, ... g a c, g a d, g b c, g b d]

  return $ P            = stream [a,b,c,d]) 
           `mplus`
          loop P''       =  grow func (P mplus P') = 
              (P mplus P')   -- try to produce a stream [g a c, g a d, g b c, g b d, ] `mplus` [a,b,c,d] 
  
  


main = do
  program <- loop ....
  guard $ check program


  

matches function:
  - uses getUnified (or similar logic)
      instead of going through all components in env, we'd go through 
      all components in P (so built up components thus far)


0. check all {components} and see if you can return them as solutions
1. grow {components} to get {components + more things} ??? ?How though
2. repeat



0   1
x + sort(x)

---------------------------
T - terminals 
N - non-terminals
R - rules (productions)
S - starting nonterminal

Nullary: 
In computer programming, a nullary constructor is a constructor 
that takes no arguments. Also known as a 0-argument constructor 
or no-argument constructors. 

bottom-up (<T, N, R, S>, [i→o]) {         dfs T = do
  P := [t | t in T && t is nullary]         let P = [t | t <- T, isNullary t]
  while(true)                               results <- do
    forall(p in P)                            p <- P -- list monad
      if(whole(p) && p([i]) = [o])            guard $ (whole p && check p)
        return p;                             return p
    P += grow(P);                           return $ results ++ dfs (grow P)
}

looks like a fold over R
grow 

grow (P) {                                grow P = do
  P’ := []                                  let P' = []
  forall((_ ::= rhs) in R)                  comp <- components (from env)
    P’ += [rhs[B -> p] | p in P, B→∗𝑝]      ?????????????????????????????????????????
  return P’;                                return P'
}



---------------------------


-}







type BottomUpSolver m = StateT CheckerState (LogicT m)

evalBottomUpSolver :: Monad m => Chan Message -> BottomUpSolver m a -> m a
evalBottomUpSolver messageChan m = do
  observeT $ m `evalStateT` emptyChecker {_checkerChan = messageChan}

evalBottomUpSolverList :: Monad m => Chan Message -> [BottomUpSolver m a] -> m a
evalBottomUpSolverList messageChan m = do
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m


-- TODO this
dothing :: Monad m => Environment -> BottomUpSolver m a
dothing env = do

  -- collect all the component types (which we might use to fill the holes)
  component <- choices $ Map.toList (env ^. symbols)
  
  -- stream of components that unify with goal type
  growedProgram <- grow component :: BottomUpSolver IO RProgram
  undefined
  
  where
    -- fill in args of schema using the ground programs we already have
    -- (instead of calling dfs on each argument)
    -- g ?? ?? ??    <- fill these holes :: SType
    --    using programs in P
    --    returning a program
    grow :: (Id, RSchema) -> BottomUpSolver IO RProgram
    grow    comp@(id, schema) = do
      -- stream of solutions to the (id, schema) returned from getUnifiedComponent
      if isGround schema
        then return Program { content = PSymbol id, typeOf = refineTop env schema }
        else do
          -- collect all the argument types (the holes ?? we need to fill)
          let args = allArgTypes schema :: [SType]

          -- TODO change to matching everything from p that it unifies with
          let func = getUnifiedComponent ... :: SType -> RProgram
          argsFilled <- mapM func args :: BottomUpSolver IO [RProgram]
          return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 
        
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- TODO change this (/ make sure it's right)
    --   i.e. make it into this type or something: SType -> RProgram
    -- get every program from P that unifies with goal
    getUnifiedComponent :: SType -> (Id, RSchema) -> BottomUpSolver IO (Id, SType)
    getUnifiedComponent goalType (id, schema) = do
      
      -- replaces "a" with "tau1"
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: BottomUpSolver IO ()
      st' <- get
      
      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      
      guard checkResult
      return (id, stypeSubstitute sub (shape freshVars))









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
