{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Types where

import Control.Lens
import Control.Monad.State
import Control.Monad.Logic
import Control.Concurrent.Chan
import Types.Experiments
import TopDown.GoalTrace
import TopDown.Size
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Map (Map)
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Program
import Types.TypeChecker
import Types.Type
import System.IO

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
              -- prog      size sub           nameCounter 
-- type MemoList = [(RProgram, Int, Map Id SType, Map Id Int)]
-- type MemoValue = (MemoList, Bool)
type MemoValue = [RProgram]
type MemoMap = Map MemoKey MemoValue
type ArgsMap = Map Id RType
data DebugEnv = DebugEnv {
    _logHandle :: Maybe Handle, -- if Nothing, it means debug is disabled
    _dfsCounter :: Int
  }
makeLenses ''DebugEnv

isDebugEnabled :: Monad m => StateT DebugEnv m Bool
isDebugEnabled = isJust <$> use logHandle

incrementDfsCounter :: Monad m => StateT DebugEnv m ()
incrementDfsCounter = modifying dfsCounter (+1)

type TopDownSolver m = StateT CheckerState (StateT ArgsMap (StateT GoalTrace (LogicT (StateT MemoMap (StateT DebugEnv m)))))


evalTopDownSolver :: forall a. SearchParams -> Chan Message -> RType -> [TopDownSolver IO a] -> IO a
evalTopDownSolver searchParams messageChan goalType m = do
  let comp = evalMemoMap . observeT $ msum $ map (evalGoalTrace . evalArgsMap . evalCheckerState) m :: StateT DebugEnv IO a
  let enableDebug = _topDownEnableDebug searchParams
  if enableDebug
    then withFile "top_down.txt" WriteMode $ \handle -> evalStateT comp (DebugEnv { _logHandle = Just handle, _dfsCounter = 0 })
    else evalStateT comp (DebugEnv { _logHandle = Nothing, _dfsCounter = 0 })
  where
    evalCheckerState = (`evalStateT` emptyChecker {_checkerChan = messageChan}) -- for StateT CheckerState
    evalArgsMap = (`evalStateT` Map.empty)                                      -- for StateT ArgsMap
    evalGoalTrace = (`evalStateT` [mkHole goalType])                            -- for StateT GoalTrace
    evalMemoMap = (`evalStateT` Map.empty) . printMemoMap'                      -- for StateT MemoMap

    -- temporary (tm)
    printMemoMap' :: StateT MemoMap (StateT DebugEnv m) a -> StateT MemoMap (StateT DebugEnv m) a
    printMemoMap' = id
    -- printMemoMap' m = m >>= (\ret -> do
    --   memoMap <- get
    --   lift $ printMemoMap memoMap
    --   return ret)

liftDebug :: Monad m => StateT DebugEnv m a -> TopDownSolver m a
liftDebug = lift . lift . lift . lift . lift

liftMemo :: Monad m => StateT MemoMap (StateT DebugEnv m) a -> TopDownSolver m a
liftMemo = lift . lift . lift . lift

liftGoalTrace :: Monad m => StateT GoalTrace (LogicT (StateT MemoMap (StateT DebugEnv m))) a -> TopDownSolver m a
liftGoalTrace = lift . lift

liftArgs :: Monad m => StateT ArgsMap (StateT GoalTrace (LogicT (StateT MemoMap (StateT DebugEnv m)))) a -> TopDownSolver m a
liftArgs = lift

-- | convert to Logic a
choices :: (Traversable f, MonadPlus m) => f a -> m a
choices = msum . fmap return
