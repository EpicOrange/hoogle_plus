{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Types(SynMode(..), MemoKey(..), MemoValue, MemoMap, ArgsMap, TopDownSolver, evalTopDownSolver, liftMemo, liftGoalTrace, liftArgs, printMemoMap, printSub, choices) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Logic
import Control.Concurrent.Chan
import Text.Printf
import Types.Experiments
import TopDown.GoalTrace
import TopDown.Size
import qualified Data.Map as Map
import Data.Map (Map)
import Types.CheckMonad
import Types.Common
import Types.Program
import Types.TypeChecker
import Types.Type


data SynMode = IMode | EMode deriving (Eq, Ord, Show)
data MemoKey = MemoKey {
    _mode :: SynMode,
    _goalType :: RType,
    _progSize :: Int,
    _args :: Map Id RType
    -- _sub :: Map Id SType
  } deriving (Eq, Ord, Show)

-- (result of query, is the result partial (False) or fully evaluated (True)?)
-- result is a tuple (prog, sub, nameCounter)
-- TODO maybe put list into an object
type MemoValue = ([(RProgram, Int, Map Id SType, Map Id Int)], Bool)
type MemoMap = Map MemoKey MemoValue
type ArgsMap = Map Id RType
type TopDownSolver m = StateT CheckerState (StateT ArgsMap (StateT GoalTrace (LogicT (StateT MemoMap m))))

evalTopDownSolver :: forall a. RType -> Chan Message -> [TopDownSolver IO a] -> IO a
evalTopDownSolver goalType messageChan m =
  (`evalStateT` Map.empty) $ printMemoMap' $ observeT $ msum $ map (evalGoalTrace . evalArgsMap . evalCheckerState) m
  where
    evalCheckerState = (`evalStateT` emptyChecker {_checkerChan = messageChan}) -- for StateT CheckerState
    evalArgsMap = (`evalStateT` Map.empty)                                      -- for StateT ArgsMap
    evalGoalTrace = (`evalStateT` [mkHole goalType])                            -- for StateT GoalTrace

    -- temporary (tm)
    printMemoMap' :: StateT MemoMap IO a -> StateT MemoMap IO a
    printMemoMap' = id
    -- printMemoMap' m = m >>= (\ret -> do
    --   memoMap <- get
    --   lift $ printMemoMap memoMap
    --   return ret)

liftMemo :: Monad m => LogicT (StateT MemoMap m) a -> TopDownSolver m a
liftMemo = lift . lift . lift

liftGoalTrace :: Monad m => StateT GoalTrace (LogicT (StateT MemoMap m)) a -> TopDownSolver m a
liftGoalTrace = lift . lift

liftArgs :: Monad m => StateT ArgsMap (StateT GoalTrace (LogicT (StateT MemoMap m))) a -> TopDownSolver m a
liftArgs = lift

-- | convert to Logic a
choices :: (Traversable f, MonadPlus m) => f a -> m a
choices = msum . fmap return

printMemoMap :: MemoMap -> IO ()
printMemoMap memoMap = do
  printf "current memo map: {\n"
  mapM_ printListItem (Map.toList memoMap)
  printf "\t}\n\n"
  where
    printListItem :: (MemoKey, MemoValue) -> IO ()
    printListItem (key, (list, isComplete)) = do
      printf "\t\t* (%s @ size %s), mode: %s, args: %s ==> [\n" (show $ _goalType key) (show $ _progSize key) (show $ _mode key) (show $ _args key)
      mapM_ (\(prog, subSize, sub, storedNameCounter) -> printf "\t\t\t(sub size %s) %s, %s, %s\n" (show subSize) (show prog) (show sub) (show storedNameCounter)) list 
      printf "\t\t] %s\n" (if isComplete then "COMPLETE" else "not complete")

printSub :: (MonadIO m) => StateT CheckerState m ()
printSub = do
  liftIO $ printf "sub = {\n"
  sub <- use typeAssignment
  liftIO $ mapM_ (\(id, t) -> printf "\t%s ==> %s (size %d)\n" id (show t) (sizeOfType t)) (Map.toList sub) --- * tau ==> Int
  -- subSize <- lift $ get
  -- liftIO $ printf "      } (size %d)\n\n" (subSize)
  liftIO $ printf "      }\n\n"
  
