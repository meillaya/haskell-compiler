module WYAH.Type
  ( TVar(..)
  , Type(..)
  , normalizeTypeVariables
  , prettyType
  ) where

data TVar = TV Int
  deriving (Eq, Ord)

data Type
  = TInt
  | TBool
  | TVariable TVar
  | TFun Type Type
  deriving (Eq, Ord)

instance Show TVar where
  show (TV n) = "t" <> show n

instance Show Type where
  show = prettyType

prettyType :: Type -> String
prettyType ty = case ty of
  TInt -> "Int"
  TBool -> "Bool"
  TVariable tv -> show tv
  TFun left right -> leftDoc left <> " -> " <> prettyType right
  where
    leftDoc left = case left of
      TFun _ _ -> "(" <> prettyType left <> ")"
      _ -> prettyType left

normalizeTypeVariables :: Type -> Type
normalizeTypeVariables ty = let (normalized, _, _) = go [] 0 ty in normalized
  where
    go mapping nextId current = case current of
      TInt -> (TInt, mapping, nextId)
      TBool -> (TBool, mapping, nextId)
      TVariable tv ->
        case lookup tv mapping of
          Just renamed -> (TVariable renamed, mapping, nextId)
          Nothing ->
            let renamed = TV nextId
             in (TVariable renamed, mapping <> [(tv, renamed)], nextId + 1)
      TFun left right ->
        let (left', mapping', nextId') = go mapping nextId left
            (right', mapping'', nextId'') = go mapping' nextId' right
         in (TFun left' right', mapping'', nextId'')
