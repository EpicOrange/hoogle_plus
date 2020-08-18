{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopDown.Size(sizeOfProg, sizeOfType, sizeOfSub) where

import Control.Lens
import Control.Monad.State
import Types.CheckMonad
import Types.Common
import Types.Program
import Types.TypeChecker
import Types.Type
import Synquid.Pretty
import qualified Data.Map as Map
import Data.Map (Map)

-- gets the size of a program
sizeOfProg :: RProgram -> Int
sizeOfProg p = case content p of
    PSymbol _       -> 1
    PApp _ ps       -> 1 + sum (map sizeOfProg ps)
    PFun _ p1       -> 1 + sizeOfProg p1
    _               -> error $ "sizeOfProg doesn't support: " ++ (show p)

-- gets the size of a type
-- ScalarT examples: 
--   Bool                          (size 1)
--   Maybe Int                     (size 2)
--   Either Int Int                (size 3)
-- FunctionT examples: 
--   tau1 -> tau2                  (size 2)
--   tau1 -> (tau2 -> tau3)        (size 3)
--   tau1 -> (tau2 -> Maybe b)     (size 4)
-- DatatypeT examples:
--   Int                           (size 1)
--   Maybe Int                     (size 2)
--   Either (Either Int Bool) Int  (size 5)
-- TypeVarT examples:
--   tau0                          (size 1)
--   tau1                          (size 1)
--   tau2                          (size 1)
sizeOfType :: TypeSkeleton r -> Int
sizeOfType t =
  case t of
    ScalarT baseType _ -> sizeOfBase baseType
    FunctionT _ fromType toType -> sizeOfType fromType + sizeOfType toType
    _ -> error $ "bad type given to TopDown.Size.sizeOfType"
  where
    sizeOfBase :: BaseType r -> Int
    sizeOfBase t' =
      case t' of
        (DatatypeT _ args _) -> 1 + (sum $ map sizeOfType args)
        (TypeVarT _ _) -> 1
        _ -> error $ "bad type given to TopDown.Size.sizeOfBase"

-- gets the size of the typeAssignment substitution map
sizeOfSub :: (Monad m) => StateT CheckerState m Int
sizeOfSub = do
  st <- get
  let sub = st ^. typeAssignment :: Map Id SType
  return $ Map.foldr f 0 sub
  where
    f :: SType -> Int -> Int
    f t acc = sizeOfType t + acc