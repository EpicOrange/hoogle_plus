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
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either
import Data.List
import Text.Printf

import Text.Parsec.Pos (initialPos)
import Text.Parsec.Indent (runIndentParserT)

-- | check if these particular parameters to dfs has been memoized
-- | if not, run dfs (compute) and add to memo map
memoizeProgram :: SynMode -> Int -> RType -> Int -> TopDownSolver IO (RProgram, Int) -> TopDownSolver IO (RProgram, Int)
memoizeProgram mode quota goalType depth compute = do
  
  -- -- debug
  -- when False $ do
  --   memoMap <- liftMemo get
  --   liftIO $ printf "========quota %d=========\n" quota
  --   liftIO $ printMemoMap memoMap
  --   debugGoalTrace =<< liftGoalTrace get
  --   printSub
  --   liftIO $ printf "========================\n"

  -- TODO make the goal have betas now instead of alpha's / tau's

  -- construct the key
  sub <- use typeAssignment
  let applySub = addTrue . stypeSubstitute sub . shape
  -- let subbedGoal = applySub goalType
  args <- liftArgs get
  let subbedArgs = Map.map applySub args
  
  let key = MemoKey mode goalType quota args

  -- do the lookup
  memoMap <- liftMemo get
  case Map.lookup key memoMap of
    Just stored -> retrieve stored -- found some stored programs, so return them
    Nothing     -> evaluate key    -- found no stored programs, so we need to run compute to generate them

  where
    -- | return the stored programs and deal with setting typeAssignment
    retrieve :: MemoValue -> TopDownSolver IO (RProgram, Int)
    retrieve (list, isComplete) = do
      sub <- use typeAssignment
      guard isComplete

      log depth $ printf "GOAL: (%s | quota %d | ?? :: %s) has %d solution%s in memo map\n"
        (show mode) quota (show goalType) (length list) (if length list == 1 then "" else "s")

      (prog, subSize, savedSub, storedNameCounter) <- choices list
      
      log (depth+1) $ printf "retrieved (size %d): %s :: %s\n" (quota + subSize) (show prog) (show $ typeOf prog)
      -- liftIO $ printf "retrieved (size %d): %s :: %s\n" (quota + subSize) (show prog) (show $ typeOf prog)

      assign typeAssignment =<< unifySub sub savedSub
      
      curNameCounter <- use nameCounter
      let nameCounter' = Map.unionWith max curNameCounter storedNameCounter
      assign nameCounter nameCounter'
      
      return (prog, subSize)
     
    -- | runs compute, storing every program as it goes, sets isComplete to true at the end
    -- | we store only the relevant part of the typeAssignment that turns goal ==> program type
    evaluate :: MemoKey -> TopDownSolver IO (RProgram, Int)
    evaluate key = do
        -- (will reenumerate all programs and possibly change memoMap)
        beforeSub <- use typeAssignment
        log depth $ printf "GOAL: (%s | quota %d | ?? :: %s) is being seen for the first time\n" (show mode) quota (show goalType)

        (prog, subSize) <- compute `mplus` setComplete key
        afterSub <- use typeAssignment

        -- keep only type substitutions that were added by running compute
        let updatedSub = afterSub `Map.difference` beforeSub

        -- store updatedSub, nameCounter in the map

        -- construct a key
        progSize <- sizeOfProg prog subSize
        progNameCounter <- use nameCounter
        args <- liftArgs get
        
        when (progSize /= quota) $ error "progsize (%d) and quota (%d) should be the same for \n\tquery: %s\n\tprogram: %s\n" (progSize) (quota) (show goalType) (prog)
        
        let key' = MemoKey mode goalType progSize args

        memoMap <- liftMemo get :: TopDownSolver IO MemoMap
        let add :: RProgram -> MemoMap
            add prog = do
              let storedValue = (prog, subSize, updatedSub, progNameCounter)
              let f Nothing                   = Just ([storedValue], False)
                  f (Just (list, isComplete)) = Just (storedValue:list, isComplete)
              Map.alter f key' memoMap
        
        -- we want to add this prog to the existing memo map if possible
        -- there's 3 situations
        --   1. key is not in the map
        --      -> we want to add and return prog
        --   2. (key ==> list) is in the map, and prog is already in the list
        --      -> we want to not return prog (do nothing) --- guard
        --   3. (key ==> list) is in the map, and prog is not in the list
        --      -> we want to append and return prog
        log (depth+1) $ printf "memoized! (%s | quota %d | ?? :: %s) ==> %s :: %s, via sub %s\n" (show mode) quota (show goalType) (show prog) (show $ typeOf prog) (showMap updatedSub)
        
        case Map.lookup key' memoMap of
          -- add the entry to our memo map!
          Nothing -> do
            liftMemo $ put $ add prog
            return (prog, subSize)
          
          Just (list, isComplete) -> do
            -- see if there's an existing (prog, sub) in the list (ignoring nameCounter)
            -- if so, update the nameCounter to be the maximum of itself and progNameCounter
            -- case find (\(p,s,u,c) -> (p,s) == (prog, subSize)) list of
            case find (\(p,s,u,c) -> (p,s,u,c) == (prog, subSize, updatedSub, progNameCounter)) list of
              -- if it isn't in the completed list, that's wrong because isComplete means everything's in the list
              Nothing | isComplete -> do
                memoMap <- liftMemo get
                debugGoalTrace =<< liftGoalTrace get
                liftIO $ printf "list: \n"
                liftIO $ mapM_ (\t -> printf "\t* %s\n" (show t)) list

                error $ do
                  printf "goal: %s\n%s says complete but isn't there\n" (show goalType) (show (prog, subSize, updatedSub, progNameCounter))
                  -- let err1 = printf "oops... (%s) %s @ size %s (%s + subsize %s) says complete but isn't there: \n\t%s :: %s\n" (show mode) (show goalType) (show progSize) (show $ sizeOfContent prog) (show subSize) (show prog) (show $ typeOf prog)
                  -- let err2 = printf "\tsub: %s\n\tnameCounter: %s\n\targs: %s\n\tbeforeSub: %s\n\tafterSub: %s\n" (show $ Map.toList updatedSub) (show $ Map.toList progNameCounter) (show $ args) (show $ beforeSub) (show $ afterSub)
                  -- err1 ++ err2

              -- if it isn't in the list but the list is incomplete, add it
              Nothing      -> do
                liftMemo $ put $ add prog
                -- liftIO $ printf "\t### adding program %s to (size %d) goal %s\n" (show prog) (quota) (show goalType)
                return (prog, subSize)

              -- if (prog, sub) is already in the list, update the nameCounter to be the maximum of itself and progNameCounter
              Just thing@(_,_,u,c) -> do
                -- error "I feel like this shouldn't happen either... TYPETHIS to find"
                -- memoMap <- liftMemo get
                -- liftIO $ printMemoMap memoMap
                liftIO $ printf "list: \n"
                liftIO $ mapM_ (\t -> printf "\t* %s\n" (show t)) list
                error $ do
                  let err1 = printf "oops... I feel like this shouldn't happen either...TYPETHIS to find\n"
                  let err2 = printf "\tmode (%s), goalType: %s\n" (show mode) (show goalType)
                  let err3 = printf "\tsub: %s\n\tnameCounter: %s\n\targs: %s\n\tbeforeSub: %s\n\tafterSub: %s\n" (show $ Map.toList updatedSub) (show $ Map.toList progNameCounter) (show $ args) (show $ beforeSub) (show $ afterSub)
                  err1 ++ err2 ++ err3

                -- let s1 = u
                -- let s2 = updatedSub
                -- -- get the most specific sub combination
                -- unionedSub <- unifySub s1 s2
                -- let list' = map (\(p,s,u,c) -> if (p,s) == (prog, subSize) then (p, s, unionedSub, Map.unionWith max c progNameCounter) else (p,s,u,c)) list
                -- liftMemo $ put $ Map.insert key' (list', isComplete) memoMap
                -- return (prog, subSize)

        -- case Map.lookup key' memoMap of
        --   -- add the entry to our memo map!
        --   Nothing -> do
        --     liftMemo $ put $ add prog
        --     return (prog, subSize)
        --   Just (list, isComplete) -> do
        --     -- see if there's an existing (prog, sub) in the list (ignoring nameCounter)
        --     -- if so, update the nameCounter to be the maximum of itself and progNameCounter
        --     case find (\(p,s,u,c) -> (p,s) == (prog, subSize)) list of
        --       -- if it isn't in the completed list, that's wrong because isComplete means everything's in the list
        --       Nothing | isComplete -> do
        --         memoMap <- liftMemo get
        --         debugGoalTrace =<< liftGoalTrace get
        --         error $ do
        --           let err1 = printf "oops... (%s) %s @ size %s (%s + subsize %s) says complete but isn't there: \n\t%s :: %s\n" (show mode) (show goalType) (show progSize) (show $ sizeOfContent prog) (show subSize) (show prog) (show $ typeOf prog)
        --           let err2 = printf "\tsub: %s\n\tnameCounter: %s\n\targs: %s\n\tbeforeSub: %s\n\tafterSub: %s\n" (show $ Map.toList updatedSub) (show $ Map.toList progNameCounter) (show $ args) (show $ beforeSub) (show $ afterSub)
        --           err1 ++ err2

        --       -- if it isn't in the list but the list is incomplete, add it
        --       Nothing      -> do
        --         liftMemo $ put $ add prog
        --         -- liftIO $ printf "\t### adding program %s to (size %d) goal %s\n" (show prog) (quota) (show goalType)
        --         return (prog, subSize)

        --       -- if (prog, sub) is already in the list, update the nameCounter to be the maximum of itself and progNameCounter
        --       Just (_,_,u,c) -> do
        --         let s1 = u
        --         let s2 = updatedSub
        --         -- get the most specific sub combination
        --         unionedSub <- unifySub s1 s2
        --         let list' = map (\(p,s,u,c) -> if (p,s) == (prog, subSize) then (p, s, unionedSub, Map.unionWith max c progNameCounter) else (p,s,u,c)) list
        --         liftMemo $ put $ Map.insert key' (list', isComplete) memoMap
        --         return (prog, subSize)

    -- | set the complete flag and return mzero, called when we're done with compute
    setComplete :: MemoKey -> TopDownSolver IO (RProgram, Int)
    setComplete key = do
      log depth $ printf "memoize marks this goal as COMPLETE: (%s | quota %d | ?? :: %s)\n" (show mode) quota (show goalType)
      liftMemo $ modify $ Map.insertWith (\_ (list, isComplete) -> (list, True)) key ([], True)
      mzero
