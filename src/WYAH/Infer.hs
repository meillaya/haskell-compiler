module WYAH.Infer
  ( TypeError(..)
  , inferExpr
  ) where

import Data.List (nub, (\\))
import WYAH.Name
import WYAH.Span (Span, renderSpan)
import WYAH.Syntax (Literal(..))
import WYAH.Type

data TypeError = TypeError
  { typeErrorSpan :: Span
  , typeErrorMessage :: String
  }
  deriving (Eq, Show)

data Scheme = Forall [TVar] Type
  deriving (Eq, Show)

type Subst = [(TVar, Type)]
type Env = [(ResolvedName, Scheme)]

inferExpr :: RenamedExpr -> Either TypeError Type
inferExpr expr = do
  (_, ty, _) <- infer [] 0 expr
  pure ty

infer :: Env -> Int -> RenamedExpr -> Either TypeError (Subst, Type, Int)
infer env nextId expr = case expr of
  RLit _ literal ->
    let ty = case literal of
          LitInt _ -> TInt
          LitBool _ -> TBool
     in Right ([], ty, nextId)
  RVar sp name ->
    case lookup name env of
      Just scheme ->
        let (ty, nextId') = instantiate nextId scheme
         in Right ([], ty, nextId')
      Nothing -> Left (TypeError sp ("internal inference error: missing renamed binding for " <> show name))
  RLam _ name body -> do
    let argType = TVariable (TV nextId)
    (substBody, bodyType, nextId') <- infer ((name, Forall [] argType) : env) (nextId + 1) body
    let inputType = apply substBody argType
    Right (substBody, TFun inputType bodyType, nextId')
  RApp sp fun arg -> do
    (substFun, funType, nextId') <- infer env nextId fun
    let env' = applyEnv substFun env
    (substArg, argType, nextId'') <- infer env' nextId' arg
    let resultType = TVariable (TV nextId'')
    substOut <- unify sp (apply substArg funType) (TFun argType resultType)
    let subst = compose substOut (compose substArg substFun)
    Right (subst, apply substOut resultType, nextId'' + 1)
  RLet _ name bound body -> do
    (substBound, boundType, nextId') <- infer env nextId bound
    let env' = applyEnv substBound env
        scheme = generalize env' (apply substBound boundType)
        env'' = (name, scheme) : env'
    (substBody, bodyType, nextId'') <- infer env'' nextId' body
    let subst = compose substBody substBound
    Right (subst, bodyType, nextId'')
  RIf sp cond yes no -> do
    (substCond, condType, nextId') <- infer env nextId cond
    substBool <- unify sp condType TBool
    let env' = applyEnv (compose substBool substCond) env
    (substYes, yesType, nextId'') <- infer env' nextId' yes
    let env'' = applyEnv substYes env'
    (substNo, noType, nextId''') <- infer env'' nextId'' no
    substBranches <- unify sp (apply substNo yesType) noType
    let subst = compose substBranches (compose substNo (compose substYes (compose substBool substCond)))
    Right (subst, apply substBranches noType, nextId''')

instantiate :: Int -> Scheme -> (Type, Int)
instantiate nextId (Forall vars ty) =
  let (subst, nextId') = buildSubst [] nextId vars
   in (apply subst ty, nextId')
  where
    buildSubst subst current [] = (subst, current)
    buildSubst subst current (var : rest) =
      buildSubst (subst <> [(var, TVariable (TV current))]) (current + 1) rest

generalize :: Env -> Type -> Scheme
generalize env ty = Forall quantified ty
  where
    quantified = nub (ftvType ty \\ ftvEnv env)

ftvEnv :: Env -> [TVar]
ftvEnv env = nub (concatMap (ftvScheme . snd) env)

ftvScheme :: Scheme -> [TVar]
ftvScheme (Forall quantified ty) = ftvType ty \\ quantified

ftvType :: Type -> [TVar]
ftvType ty = case ty of
  TInt -> []
  TBool -> []
  TVariable tv -> [tv]
  TFun left right -> nub (ftvType left <> ftvType right)

applyEnv :: Subst -> Env -> Env
applyEnv subst = map (fmap (applyScheme subst))

applyScheme :: Subst -> Scheme -> Scheme
applyScheme subst (Forall quantified ty) =
  let filtered = filter ((`notElem` quantified) . fst) subst
   in Forall quantified (apply filtered ty)

apply :: Subst -> Type -> Type
apply subst ty = case ty of
  TInt -> TInt
  TBool -> TBool
  TVariable tv -> maybe (TVariable tv) (apply subst) (lookup tv subst)
  TFun left right -> TFun (apply subst left) (apply subst right)

compose :: Subst -> Subst -> Subst
compose newer older = [(tv, apply newer ty) | (tv, ty) <- older] <> newer

unify :: Span -> Type -> Type -> Either TypeError Subst
unify sp left right = case (left, right) of
  (TInt, TInt) -> Right []
  (TBool, TBool) -> Right []
  (TVariable tv, ty) -> bind sp tv ty
  (ty, TVariable tv) -> bind sp tv ty
  (TFun leftIn leftOut, TFun rightIn rightOut) -> do
    substIn <- unify sp leftIn rightIn
    substOut <- unify sp (apply substIn leftOut) (apply substIn rightOut)
    Right (compose substOut substIn)
  _ -> Left (TypeError sp ("cannot unify " <> prettyType left <> " with " <> prettyType right <> " at " <> renderSpan sp))

bind :: Span -> TVar -> Type -> Either TypeError Subst
bind sp tv ty
  | ty == TVariable tv = Right []
  | occurs tv ty = Left (TypeError sp ("occurs check failed: " <> show tv <> " in " <> prettyType ty <> " at " <> renderSpan sp))
  | otherwise = Right [(tv, ty)]

occurs :: TVar -> Type -> Bool
occurs tv ty = case ty of
  TInt -> False
  TBool -> False
  TVariable other -> tv == other
  TFun left right -> occurs tv left || occurs tv right
