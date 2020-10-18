{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Debug where

import TopDown.GoalTrace
import TopDown.Size
import TopDown.Types
import HooglePlus.Synthesize

import Control.Lens
import Control.Monad.State
import Control.Concurrent.Chan
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Program
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.IOFormat
import PetriNet.Util
import Synquid.Pretty
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Text.Printf
import System.IO

printMemoMap :: MemoMap -> IO ()
printMemoMap memoMap = do
  printf "current memo map: {\n"
  mapM_ printListItem (Map.toList memoMap)
  printf "\t}\n\n"
  where
    printListItem :: (MemoKey, MemoValue) -> IO ()
    printListItem (key, list) = do
      printf "\t\t* (%s @ size %s), mode: %s, args: %s ==> [\n" (show $ _goalType key) (show $ _progSize key) (show $ _mode key) (show $ _args key)
      mapM_ (\prog -> printf "\t\t\t%s\n" (show prog)) list 
      printf "\t\t]\n"

printSub :: (MonadIO m) => StateT CheckerState m ()
printSub = do
  sub <- use typeAssignment
  liftIO $ printf "sub = {\n"
  liftIO $ mapM_ (\(id, t) -> printf "\t%s ==> %s (size %d)\n" id (show t) (sizeOfType t)) (Map.toList sub) --- * tau ==> Int
  -- subSize <- lift $ get
  -- liftIO $ printf "      } (size %d)\n\n" (subSize)
  liftIO $ printf "      }\n\n"

-- | only prints things when we've enabled debugging
debug :: Bool -> IO () -> TopDownSolver IO ()
debug b m = do
  enableDebug <- liftDebug isDebugEnabled
  when (b && enableDebug) $ liftIO m

log :: Int -> String -> TopDownSolver IO ()
log depth msg = do
  handle <- liftDebug (use logHandle)
  case handle of
    Nothing -> return ()
    Just handle -> liftIO $ hPutStr handle (replicate (depth * 2) ' ' ++ msg)

logEcho :: String -> TopDownSolver IO ()
logEcho msg = do
  handle <- liftDebug (use logHandle)
  case handle of
    Nothing -> return ()
    Just handle -> liftIO $ hPutStr handle msg >> putStrLn msg

debugDone :: IO () -> TopDownSolver IO ()
debugDone = debug True -- (Quota %d) Done with %s!

debugMemo :: MemoMap -> TopDownSolver IO ()
debugMemo = debug True . printMemoMap

debugGoalTrace :: GoalTrace -> TopDownSolver IO ()
debugGoalTrace = debug True . printGoalTrace

showMap :: (Show k, Show v, Ord k) => Map k v -> String
showMap m = "{" ++ (intercalate ", " $ Map.elems $ Map.mapWithKey (\k v -> printf "%s ==> %s" (show k) (show v)) m) ++ "}"

showMemoMap :: MemoMap -> String
showMemoMap m = "{\n" ++ (concat $ Map.mapWithKey printer m) ++ "}\n"
  where
    printer (MemoKey mode goalType progSize args) list =
      printf "  (%s | quota %d | ?? :: %s) ==> [%s]\n"
        (show mode) progSize (show goalType)
        (intercalate ", " $ map (\prog -> printf "%s :: %s" (show prog) (show $ typeOf prog)) list)
