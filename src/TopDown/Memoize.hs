{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Memoize (memoizeProgram) where

import Prelude hiding (log)
import TopDown.Debug
import TopDown.Types
import TopDown.Size
import TopDown.GoalTrace
import TopDown.TypeChecker
import HooglePlus.TypeChecker (bottomUpCheck)
import Database.Convert (addTrue)

import Control.Lens
import Control.Monad.State
import Examples.Utils (mkPolyType)
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Program
import Types.TypeChecker
import Types.Type
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Type
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either
import Data.List
import Text.Printf

import Text.Parsec.Pos (initialPos)
import Text.Parsec.Indent (runIndentParserT)

-- relevant types are in Types.hs

-- stateless memoize! it looks like this
-- we always use treat the goal type as if its free variables are renamed to beta0, beta1, etc
-- program storage:
--   (renamed goal type, quota, mode, args) ==> [prog1, prog2, ...]
--   we store the entire list when we're done, as there's no recursive thing going on anymore as size strictly decreases
--   question: what to do about the goal <beta0> . beta0? store all possible programs? special case?
--   answer: don't worry about it, this is behavior that we'll deal with once memoize actually works
-- program retrieval:
--   (renamed goal type, quota, mode, args) ==> [prog1, prog2, ...]
--   we should be able to figure out sub and progCounter when we load a program
--   unify p1 with original goal type to get the sub

-- try implementing it!
--   generalize goal type, using sub to get the actual goal type.
--   make the key: (renamed goal type, quota, mode, args)
--   use key to do a lookup
--   + if this goal is in the map already, run retrieve
--   + otherwise, run evaluate
-- retrieve:
--   we have a list of stored programs
--   the goal is to make them consistent with the current typeAssignment before returning
--   so that memoize does the job of updating typeAssignment
--   this means that
--     (?? :: alpha -> b) (?? :: alpha)
--   could become
--     (head :: [b] -> b)  (?? :: [b])
--   notice that the second goal changes, due to the new typeAssignment!
--   in addition, this filters out programs that can't unify with the current typeAssignment
-- evaluate:
--   run dfs and return every program
--   append each new program to the list of stored programs, using the key from before
--   no need to worry about setComplete due to strictly decreasing quota:
--   when we call dfs, it can only look at memo entries that have less quota than us

-- TODO in the future
-- look at Myth paper, which details a new way to do memoize

-- | check if these particular parameters to dfs has been memoized
-- | if not, run dfs (compute) and add to memo map
memoizeProgram :: Environment -> SynMode -> Int -> RType -> Int -> TopDownSolver IO (RProgram, Int) -> TopDownSolver IO (RProgram, Int)
memoizeProgram env mode quota goalType depth compute = do
  sub <- use typeAssignment
  let applySub sub = addTrue . stypeSubstitute sub . shape

  -- convert goal type and arg types to betas
  -- 1. substitute the bound vars
  args <- liftMustHave get
  let subbedGoal = applySub sub goalType
  let subbedArgs = applySub sub <$> args
  -- after substituting the bound vars, all remaining type vars are the free ones
  -- 2. map each free var to a new beta
  let isFreeVar var = "alpha" `isPrefixOf` var || "tau" `isPrefixOf` var
  let freeVars = filter isFreeVar $ Set.toAscList $ foldMap typeVarsOf (subbedGoal : Map.elems subbedArgs)
  let makeBeta i = ScalarT (TypeVarT Map.empty ("beta" ++ show i)) ()
  let betaSub = Map.fromAscList $ zip freeVars $ map makeBeta [0..] -- map from freeVar to beta0,1,2,3 ..
  let betaGoal = applySub betaSub subbedGoal
  let betaArgs = applySub betaSub <$> subbedArgs

  -- construct our key, and branch based on the lookup
  let key = MemoKey mode betaGoal quota args -- TODO should this be betaArgs or args?
  memoMap <- liftMemo get :: TopDownSolver IO MemoMap
  case Map.lookup key memoMap of
    Just progs -> retrieve betaGoal progs -- found some stored programs, infer the type for each
    Nothing    -> evaluate betaGoal key   -- found no stored programs, run compute to generate them and store at key

  where
    retrieve :: RType -> MemoValue -> TopDownSolver IO (RProgram, Int)
    retrieve betaGoal progs = do

      -- for each stored program...
      log depth $ printf "GOAL: (%s | quota %d | ?? :: %s ~ %s) has %d solution%s in memo map\n" (show mode) quota (show goalType) (show betaGoal) (length progs) (if length progs == 1 then "" else "s")
      prog <- choices (reverse progs)

      -- infer and overwrite the type of this program (using the current typeAssignment)
      -- liftIO $ print prog
      -- prog' <- bottomUpCheck env prog
      -- guard =<< use isChecked
      
      -- -- unify this new type with the query (goalType)
      -- let t1 = shape goalType :: SType
      -- let t2 = shape $ typeOf prog' :: SType
      -- topDownSolveTypeConstraint env t1 t2
      -- guard =<< use isChecked

      -- log (depth+1) $ printf "retrieved (size %d): %s :: %s\n" (quota) (show prog') (show $ typeOf prog')

      -- return (prog', 0)

      log (depth+1) $ printf "retrieved (size %d): %s :: %s\n" (quota) (show prog) (show $ typeOf prog)

      return (prog, 0)


    evaluate :: RType -> MemoKey -> TopDownSolver IO (RProgram, Int)
    evaluate betaGoal key = do
      log depth $ printf "GOAL: (%s | quota %d | ?? :: %s ~ %s) is being seen for the first time\n" (show mode) quota (show goalType) (show betaGoal)

      -- run dfs, which returns a stream of programs
      (prog, _) <- compute

      -- append each new program to the list of stored programs, using the key from before
      -- no need to worry about setComplete due to strictly decreasing quota:
      -- when we call dfs, it can only look at memo entries that have less quota than us

      memoMap <- liftMemo get :: TopDownSolver IO MemoMap
      liftMemo $ modify $ Map.insertWith (++) key [prog]
      return (prog, 0)
