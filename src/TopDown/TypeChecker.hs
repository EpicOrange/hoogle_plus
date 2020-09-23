module TopDown.TypeChecker (topDownSolveTypeConstraint, unify, unifySub, ourFreshType) where

import Database.Convert
import Database.Util
import HooglePlus.TypeChecker
import Types.TypeChecker
import Types.Environment
import Types.Type
import Types.Program
import Types.CheckMonad
import Types.Common
import Synquid.Logic (ftrue)
import Synquid.Type
import Synquid.Pretty
import Synquid.Program
import PetriNet.Util

import Control.Monad.State
import Control.Lens
import Control.Monad.Extra
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import Text.Pretty.Simple
import Data.Maybe

-- -- bottom up check a program on the concrete type system
-- -- at the same time, keep track of the abstract type for each node
-- bottomUpCheck :: MonadIO m => Environment -> RProgram -> Checker m RProgram
-- bottomUpCheck env p@(Program (PSymbol sym) typ) = do
--     -- lookup the symbol type in current scope
--     writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
--     nameMap <- gets (view nameMapping)
--     let sym' = removeLast '_' sym
--     -- find the real name of a function
--     let name = stripSuffix $ fromMaybe sym' (Map.lookup sym' nameMap)
--     t <- findSymbol env name
--     return (Program (PSymbol sym) t)
-- bottomUpCheck env (Program (PApp f args) typ) = do
--   argResult <- checkArgs args
--   case argResult of
--     Left err -> return err
--     Right checkedArgs -> do
--       -- find the real name of a function
--       nameMap <- gets (view nameMapping)
--       let name = stripSuffix $ fromMaybe f (Map.lookup f nameMap)
--       t <- findSymbol env name 
--       writeLog 3 "bottomUpCheck" $ text "Bottom up checking function" <+> pretty f
--                                  <+> text "get type" <+> pretty t
--       -- check function signature against each argument provided
--       let argVars = map shape (allArgTypes t)
--       let checkedArgTys = map (shape . typeOf) checkedArgs
--       writeLog 3 "bottomUpCheck" $ text "Bottom up checking get arg types" <+> pretty checkedArgTys
--       mapM_ (uncurry $ solveTypeConstraint env) (zip checkedArgTys argVars)
--       -- we eagerly substitute the assignments into the return type of t
--       tass <- gets (view typeAssignment)
--       let ret = addTrue $ stypeSubstitute tass (shape $ partialReturn checkedArgs t)
--       -- if any of these checks returned false, this function application
--       -- would produce a bottom type
--       ifM (gets $ view isChecked)
--           (return $ Program (PApp f checkedArgs) ret)
--           (return $ Program (PApp f checkedArgs) BotT)
--   where
--     partialReturn (_:args) (FunctionT _ _ tRes) = partialReturn args tRes
--     partialReturn [] t = t

--     checkArgs [] = return $ Right []
--     checkArgs (arg:args) = do
--         checkedArg <- bottomUpCheck env arg
--         ifM (gets $ view isChecked)
--             (do
--                checkedArgs <- checkArgs args
--                case checkedArgs of
--                  Left err -> return $ Left err
--                  Right args' -> return $ Right (checkedArg:args')
--             )
--             (return $ Left checkedArg)
-- bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
--     writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
--     body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
--     let tBody = typeOf body'
--     let t = FunctionT x tArg tBody
--     ifM (gets $ view isChecked)
--         (return $ Program (PFun x body') t)
--         (return body')
-- bottomUpCheck env p@(Program (PFun x body) _) = do
--     writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
--     let bound = env ^. boundTypeVars
--     id <- freshId bound "A"
--     id' <- freshId bound "A"
--     let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
--     let tRet = addTrue (ScalarT (TypeVarT Map.empty id') ())
--     bottomUpCheck env (Program (PFun x body)(FunctionT x tArg tRet))
-- bottomUpCheck _ p = error $ "unhandled case for checking "
--                           ++ show p ++ "::" ++ show (typeOf p)

-- checkAssignment :: MonadIO m => Environment -> Id -> SType -> Checker m ()
-- checkAssignment env id tv = do
--     tass <- gets (view typeAssignment)
--     let typ = fromJust $ Map.lookup id tass
--     writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv
--     solveTypeConstraint env typ tv

-- allowed from type vars and type classes to unify
topDownSolveTypeConstraint :: MonadIO m => Environment -> SType -> SType -> Checker m ()
topDownSolveTypeConstraint _ AnyT _ = return ()
topDownSolveTypeConstraint _ _ AnyT = return ()
topDownSolveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _)
  | id == id' = return ()
  | isBound env id && isBound env id' = modify $ set isChecked False
  | isBound env id = do
    tass <- gets (view typeAssignment)
    if id' `Map.member` tass then checkAssignment env id' tv
                             else unify env id' tv
  | otherwise = do
    tass <- gets (view typeAssignment)
    if id `Map.member` tass 
       then checkAssignment env id tv'
       else if id' `Map.member` tass
            then checkAssignment env id' tv
            else unify env id tv'
topDownSolveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
-- topDownSolveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) (ScalarT (DatatypeT dt _ _) _)
--   | tyclassPrefix `isPrefixOf` dt = modify $ set isChecked False -- type class cannot unify with a type variable
topDownSolveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    tass <- gets (view typeAssignment)
    writeLog 3 "topDownSolveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            writeLog 3 "topDownSolveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            topDownSolveTypeConstraint env typ t
        else unify env id t
topDownSolveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = topDownSolveTypeConstraint env tv t
topDownSolveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "topDownSolveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    topDownSolveTypeConstraint env tArg tArg'
    writeLog 3 "topDownSolveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
    topDownSolveTypeConstraint env tRet tRet'
topDownSolveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' =
    modify $ set isChecked False
topDownSolveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
    topDownSolveTypeConstraint' env tArgs tArgs'
  where
    topDownSolveTypeConstraint' _ []  [] = return ()
    topDownSolveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 "topDownSolveTypeConstraint" $ text "Solving constraint" <+> pretty ty <+> text "==" <+> pretty ty'
        topDownSolveTypeConstraint env ty ty'
        checked <- gets (view isChecked)
        -- if the checking between ty and ty' succeeds, proceed to others
        when checked $ topDownSolveTypeConstraint' env tys tys'
topDownSolveTypeConstraint env t1 t2 = do
    writeLog 3 "topDownSolveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
    modify $ set isChecked False


-- | Replace all bound type variables with fresh free variables
ourFreshType :: (CheckMonad (t m), MonadIO m) => [Id] -> RSchema -> Id -> t m RType
ourFreshType bounds t id = ourFreshType' Map.empty [] t
  where
    ourFreshType' subst constraints (ForallT a sch) = do
        a' <- freshId bounds id
        ourFreshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
    ourFreshType' subst constraints (Monotype t) = return (typeSubstitute subst t)


-- solveTypeConstraint :: MonadIO m => Environment -> SType -> SType -> Checker m ()
-- solveTypeConstraint _ AnyT _ = return ()
-- solveTypeConstraint _ _ AnyT = return ()
-- solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _)
--   | id == id' = return ()
--   | isBound env id && isBound env id' = modify $ set isChecked False
--   | isBound env id = do
--     tass <- gets (view typeAssignment)
--     if id' `Map.member` tass then checkAssignment env id' tv
--                              else unify env id' tv
--   | otherwise = do
--     tass <- gets (view typeAssignment)
--     if id `Map.member` tass 
--        then checkAssignment env id tv'
--        else if id' `Map.member` tass
--             then checkAssignment env id' tv
--             else unify env id tv'
-- solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
-- -- solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) (ScalarT (DatatypeT dt _ _) _)
-- --   | tyclassPrefix `isPrefixOf` dt = modify $ set isChecked False -- type class cannot unify with a type variable
-- solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
--     tass <- gets (view typeAssignment)
--     writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
--     if id `Map.member` tass
--         then do
--             let typ = fromJust $ Map.lookup id tass
--             writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
--             solveTypeConstraint env typ t
--         else unify env id t
-- solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
-- solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
--     writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
--     solveTypeConstraint env tArg tArg'
--     writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
--     solveTypeConstraint env tRet tRet'
-- solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' =
--     modify $ set isChecked False
-- solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
--     solveTypeConstraint' env tArgs tArgs'
--   where
--     solveTypeConstraint' _ []  [] = return ()
--     solveTypeConstraint' env (ty:tys) (ty':tys') = do
--         writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty ty <+> text "==" <+> pretty ty'
--         solveTypeConstraint env ty ty'
--         checked <- gets (view isChecked)
--         -- if the checking between ty and ty' succeeds, proceed to others
--         when checked $ solveTypeConstraint' env tys tys'
-- solveTypeConstraint env t1 t2 = do
--     writeLog 3 "solveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
--     modify $ set isChecked False

-- -- | unify the type variable with some given type
-- -- add the type assignment to our state
-- unify :: MonadIO m => Environment -> Id -> SType -> Checker m ()
-- unify env v t =
--     if v `Set.member` typeVarsOf t
--       then modify $ set isChecked False
--       else do
--         tass' <- gets (view typeAssignment)
--         writeLog 3 "unify" $ text (show tass')
--         if isValidSubst tass'
--            then do
--                 modify $ over typeAssignment (Map.map (stypeSubstitute (Map.singleton v t)))
--                 tass <- gets (view typeAssignment)
--                 if isValidSubst tass then modify $ over typeAssignment (Map.insert v (stypeSubstitute tass t))
--                                      else modify $ set isChecked False
--            else modify $ set isChecked False

-- isValidSubst :: Map Id SType -> Bool
-- isValidSubst m = not $ any (\(v, t) -> v `Set.member` typeVarsOf t) (Map.toList m)


-- the reason this exists (rather than say, let sub' = s1 <> s2)
-- is to make sure we don't load a component from memoize whose type conflicts with the current sub
-- example 1: goal is b, sub is empty
--   and we load (arg1 :: alpha0) with alpha0 ==> b  --- OK
-- example 2: goal is b, sub has alpha0 ==> Maybe tau1
--   and we load (arg1 :: alpha0) with alpha0 ==> b  --- NOT OK
unifySub :: (MonadIO m, MonadPlus m) => Map Id SType -> Map Id SType -> Checker m (Map Id SType)
unifySub = unionWithM combiner
  where
    -- this is Map.unionWith, but takes in a monadic combining function
    unionWithM f = Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithMaybeAMatched f)
    -- e.g. s1 {alpha ==> a}     s2 {alpha ==> b}
    -- unify alpha with b in s1 (which is the current typeAssignment)
    -- guard isChecked
    -- and return the new value of alpha in s1
    combiner :: (MonadIO m, MonadPlus m) => Id -> SType -> SType -> StateT CheckerState m (Maybe SType)
    combiner k t1 t2 = do
      unify undefined k t2
      guard =<< use isChecked
      Map.lookup k <$> use typeAssignment