{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.GoalTrace(OurProgram(..), OurType, GoalTrace, printGoalTrace, mkHole, addLam, addApp, addAppFilled) where

import Control.Monad.IO.Class
import Control.Monad.State
import Synquid.Pretty
import Types.Program
import Types.Type
import Text.Printf

type OurType = String
type GoalTrace = [OurProgram]

printGoalTrace :: MonadIO m => GoalTrace -> m ()
printGoalTrace goalTrace = do
  liftIO $ printf "-----------------\n--- BACKTRACE ---\n-----------------\n"
  liftIO $ mapM_ print $ reverse goalTrace
  liftIO $ printf "-----------------\n"

-- The purpose of this is to be able to print out partially filled programs, like
--   (?? :: b)
--   (?? :: (tau0 -> b)) (?? :: tau0)
--   (?? :: (tau1 -> (tau0 -> b))) (?? :: tau1) (?? :: tau0)
--   Data.Maybe.fromJust (?? :: Maybe (((tau0 -> b)))) (?? :: tau0)
--   Data.Maybe.fromJust arg0 (?? :: a)
--   Data.Maybe.fromJust arg0 arg1
data OurProgram = 
    Symbol String
  | Lam String OurProgram
  | App OurProgram OurProgram
  -- | Hole [type of hole] [\hole -> the program, but using `hole` to replace this hole]
  | Hole OurType (OurProgram -> OurProgram)

instance Show OurProgram where
  show (Hole t f) = show $ f (Symbol $ printf "(?? :: %s)" t)
  show (Symbol s) = s
  show (Lam x body) = printf "(\\%s -> %s)" x (show body)
  show (App from to) = "(" ++ show from ++ " " ++ show to ++ ")"

-- create hole for initial goal
mkHole :: RType -> OurProgram
mkHole t = Hole (show t) (\hole -> hole)
-- add trace for lambda
addLam :: Monad m => String -> OurType -> StateT GoalTrace m ()
addLam argName tBody = modify $ updateGoalTrace (Hole tBody $ \hole -> Lam argName hole)
-- add trace for app
addApp :: Monad m => OurType -> OurType -> StateT GoalTrace m ()
addApp t1 t2 = modify $ updateGoalTrace (Hole t1 $ \hole1 -> Hole t2 $ \hole2 -> App hole1 hole2)
-- add trace for app, with left side filled
addAppFilled :: Monad m => RProgram -> OurType -> OurProgram -> StateT GoalTrace m ()
addAppFilled prog t2 prev = modify $ (\goalTrace -> prev `replaceHole` (Hole t2 $ \hole2 -> App (Symbol (show prog)) hole2) : goalTrace)

updateGoalTrace :: OurProgram -> [OurProgram] -> [OurProgram]
updateGoalTrace comp goalTrace = head goalTrace `replaceHole` comp : goalTrace

-- (?? :: b)
-- (?? :: (tau0 -> b)) (?? :: tau0)
-- (?? :: (tau1 -> (tau0 -> b))) (?? :: tau1) (?? :: tau0)
-- Data.Maybe.fromJust (?? :: Maybe (((tau0 -> b)))) (?? :: tau0)
-- Data.Maybe.fromJust arg0 (?? :: a) (?? :: tau1) (?? :: tau0)

-- Data.Maybe.fromJust arg0 (?? :: a)

-- We want to replace the first hole in
--   Hole _   (\hole1 -> App hole1 (Symbol "x"))
-- using
--   Hole "b" (\hole2 -> App (Symbol "f") hole2)
-- the result should look like:
--   Hole "b" (\hole2 -> App (App (Symbol "f") hole2) (Symbol "x"))
-- we have:
--   t  = "b"
--   f  = \hole1 -> App hole1 (Symbol "x")
--   f' = \hole2 -> App (Symbol "f") hole2
-- Working backwards, this should give the expected answer:
--      Hole t   (\hole2 -> f (f' hole2))
--   == Hole "b" (\hole2 -> f ((\hole2 -> App (Symbol "f") hole2) hole2))
--   == Hole "b" (\hole2 -> f (App (Symbol "f") hole2))
--   == Hole "b" (\hole2 -> (\hole1 -> App hole1 (Symbol "x")) (App (Symbol "f") hole2))
--   == Hole "b" (\hole2 -> App (App (Symbol "f") hole2) (Symbol "x"))


-- We want to replace the first hole in
--   Hole "tau1 -> b" (\hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2))
-- using
--   Hole "tau2 -> b" (\hole1' -> Hole "tau2" (\hole2' -> App hole1' hole2'))
-- the result should look like:
--   Hole "tau2 -> b" (\hole1' -> Hole "tau2" (\hole2' -> Hole "tau1" (\hole2 -> App (App hole1' hole2') hole2)))
-- we have:
--   t  = "tau2 -> b"
--   f  = \hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2)
--   f' = \hole1' -> Hole "tau2" (\hole2' -> App hole1' hole2')
-- Working backwards, this should give the expected answer:
--    Hole t            \hole1' -> replaceHole (Hole _ f) (f' hole1')
-- == Hole "tau2 -> b"  \hole1' -> replaceHole (Hole _ f) (f' hole1')
--                                       let t = "tau2"
--                                       let f = \hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2)
--                                       let f' = \hole2' -> App hole1' hole2'
--                       == ... -> Hole t      (\hole2' -> replaceHole (Hole _ f) (f' hole2'))
--                       == ... -> Hole t      (\hole2' -> f (f' hole2'))
--                       == ... -> Hole "tau2" (\hole2' -> f ((\hole2' -> App hole1' hole2') hole2'))
--                       == ... -> Hole "tau2" (\hole2' -> f (App hole1' hole2'))
--                       == ... -> Hole "tau2" (\hole2' -> (\hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2)) (App hole1' hole2'))
--                       == ... -> Hole "tau2" (\hole2' -> Hole "tau1" (\hole2 -> App (App hole1' hole2') hole2)))
-- == Hole "tau2 -> b" (\hole1' -> Hole "tau2" (\hole2' -> Hole "tau1" (\hole2 -> App (App hole1' hole2') hole2)))
replaceHole :: OurProgram -> OurProgram -> OurProgram
replaceHole   h          (Hole t f') = Hole t $ replaceHole h . f'
replaceHole   (Hole _ f) replaceWith = f replaceWith
replaceHole   h          replaceWith = error (printf "Tried to replace hole with %s in a program without a hole, %s\n" (show replaceWith) (show h))

testOurProgram :: IO ()
testOurProgram = do
  printf "test1: \t\t%s\n" (show test1)
  printf "test2: \t\t%s\n" (show test2)
  printf "test2Expected: \t%s\n" (show test2Expected)
  printf "test3: \t\t%s\n" (show test3)
  printf "test3Expected: \t%s\n" (show test3Expected)
  printf "test4: \t\t%s\n" (show test4)
  printf "test4Expected: \t%s\n" (show test4Expected)
  printf "test5: \t\t%s\n" (show test5)
  printf "test5Expected: \t%s\n" (show test5Expected)
  printf "test6: \t\t%s\n" (show test6)
  printf "test6Expected: \t%s\n" (show test6Expected)
  where
    test1 = Hole "b" (\hole1 -> hole1)
    test2 = replaceHole test1 $ Hole "tau1 -> b" (\hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2))
    test2Expected = Hole "tau1 -> b" (\hole1 -> Hole "tau1" (\hole2 -> App hole1 hole2))
    test3 = replaceHole test2 $ Hole "tau2 -> (tau1 -> b)" (\hole1 -> Hole "tau2" (\hole2 -> App hole1 hole2))
    test3Expected = Hole "tau2 -> (tau1 -> b)" (\hole1 -> Hole "tau2" (\hole2 -> Hole "tau1" (\hole2' -> App (App hole1 hole2) hole2')))
    test4 = replaceHole test3 $ Symbol "fromJust"
    test4Expected = Hole "tau2" (\hole2 -> Hole "tau1" (\hole2' -> App (App (Symbol "fromJust") hole2) hole2'))
    test5 = replaceHole test4 $ Symbol "arg0"
    test5Expected = Hole "tau1" (\hole2' -> App (App (Symbol "fromJust") (Symbol "arg0")) hole2')
    test6 = replaceHole test5 $ Symbol "arg1"
    test6Expected = App (App (Symbol "fromJust") (Symbol "arg0")) (Symbol "arg1")
