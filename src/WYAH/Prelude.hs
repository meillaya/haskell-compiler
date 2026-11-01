-- | Names that are available before the user's module is renamed.
--
-- The reference implementation compiles a @prelude.fun@ module at startup.
-- For a self-contained compiler we instead seed the renamer and type checker
-- with a fixed prelude.  The names deliberately use negative unique ids so
-- they can never collide with user binders.
module WYAH.Prelude
  ( preludeValueNames
  , preludeConstructorNames
  , preludeTypeNames
  , preludeValues
  , preludeConstructors
  , preludeLookupValue
  , preludeLookupConstructor
  , builtinName
  , preludeSchemes
  ) where

import WYAH.Renamed (ResolvedName(..))
import WYAH.Type

preludeValueNames :: [String]
preludeValueNames =
  [ "id"
  , "const"
  , "compose"
  , "flip"
  , "not"
  , "otherwise"
  , "map"
  , "filter"
  , "foldr"
  , "foldl"
  , "head"
  , "tail"
  , "null"
  , "length"
  , "++"
  , "take"
  , "drop"
  , "reverse"
  , "concat"
  , "concatMap"
  , "zip"
  , "fst"
  , "snd"
  , "error"
  , "undefined"
  , "show"
  , "succ"
  , "pred"
  , "iszero"
  , "add"
  , "negate"
  , "div"
  , "mod"
  , "quot"
  , "rem"
  , "even"
  , "odd"
  , "enumFrom"
  , "enumFromThen"
  , "enumFromTo"
  , "enumFromThenTo"
  , "seq"
  , "fix"
  , "+"
  , "-"
  , "*"
  , "/"
  , "=="
  , "/="
  , "<"
  , "<="
  , ">"
  , ">="
  , "&&"
  , "||"
  , "."
  , "$"
  , "print"
  , "putStr"
  , "putStrLn"
  , "getLine"
  , "return"
  , ">>="
  , ">>"
  , "fail"
  ]

preludeConstructorNames :: [String]
preludeConstructorNames =
  [ "False"
  , "True"
  , "[]"
  , ":"
  , "()"
  , "(,)"
  , "(,,)"
  , "(,,,)"
  , "Nothing"
  , "Just"
  , "Left"
  , "Right"
  ]

-- | Type constructors that are in scope without a data declaration.
preludeTypeNames :: [String]
preludeTypeNames =
  [ "Int"
  , "Bool"
  , "Char"
  , "String"
  , "()"
  , "[]"
  , "(,)"
  , "(,,)"
  , "(,,,)"
  , "(->)"
  , "IO"
  , "Maybe"
  , "Either"
  , "Int#"
  , "Char#"
  , "Double#"
  , "Addr#"
  ]

preludeValues :: [(String, ResolvedName)]
preludeValues =
  [ (name, ResolvedName name (-1 - index))
  | (index, name) <- zip [0 ..] preludeValueNames
  ]

preludeConstructors :: [(String, ResolvedName)]
preludeConstructors =
  [ (name, ResolvedName name (-1000 - index))
  | (index, name) <- zip [0 ..] preludeConstructorNames
  ]

preludeLookupValue :: String -> Maybe ResolvedName
preludeLookupValue name = lookup name preludeValues

preludeLookupConstructor :: String -> Maybe ResolvedName
preludeLookupConstructor name = lookup name preludeConstructors

-- | Construct a stable prelude name directly.  Used by the type checker and
-- evaluator to refer to built-in functions without threading an environment.
builtinName :: String -> ResolvedName
builtinName name =
  case preludeLookupValue name of
    Just resolved -> resolved
    Nothing ->
      case preludeLookupConstructor name of
        Just resolved -> resolved
        Nothing -> ResolvedName name (-999999)

-- Built-in type schemes -----------------------------------------------------

intTy :: Type
intTy = TCon (TyCon "Int" KStar 0)

boolTy :: Type
boolTy = TCon (TyCon "Bool" KStar 0)

charTy :: Type
charTy = TCon (TyCon "Char" KStar 0)

unitTy :: Type
unitTy = TCon (TyCon "()" KStar 0)

listCon :: TyCon
listCon = TyCon "[]" (KArrow KStar KStar) 1

tuple2Con :: TyCon
tuple2Con = TyCon "(,)" (KArrow KStar (KArrow KStar KStar)) 2

ioCon :: TyCon
ioCon = TyCon "IO" (KArrow KStar KStar) 1

listOf :: Type -> Type
listOf element = TApp (TCon listCon) element

tupleOf2 :: Type -> Type -> Type
tupleOf2 left right = TApp (TApp (TCon tuple2Con) left) right

ioOf :: Type -> Type
ioOf value = TApp (TCon ioCon) value

stringTy :: Type
stringTy = listOf charTy

tvar :: Int -> String -> TVar
tvar uniqueId name = freshTypeVariable uniqueId name KStar

fun :: Type -> Type -> Type
fun = TFun

scheme :: [TVar] -> [Pred] -> Type -> Scheme
scheme = Forall

preludeSchemes :: [(ResolvedName, Scheme)]
preludeSchemes =
  [ (builtinName "id", scheme [aVar] [] (fun a a))
  , (builtinName "const", scheme [aVar, bVar] [] (fun a (fun b a)))
  , (builtinName "compose", scheme [aVar, bVar, cVar] [] (fun (fun b c) (fun (fun a b) (fun a c))))
  , (builtinName "flip", scheme [aVar, bVar, cVar] [] (fun (fun a (fun b c)) (fun b (fun a c))))
  , (builtinName "not", scheme [] [] (fun boolTy boolTy))
  , (builtinName "otherwise", scheme [] [] boolTy)
  , (builtinName "map", scheme [aVar, bVar] [] (fun (fun a b) (fun (listOf a) (listOf b))))
  , (builtinName "filter", scheme [aVar] [] (fun (fun a boolTy) (fun (listOf a) (listOf a))))
  , (builtinName "foldr", scheme [aVar, bVar] [] (fun (fun a (fun b b)) (fun b (fun (listOf a) b))))
  , (builtinName "foldl", scheme [aVar, bVar] [] (fun (fun b (fun a b)) (fun b (fun (listOf a) b))))
  , (builtinName "head", scheme [aVar] [] (fun (listOf a) a))
  , (builtinName "tail", scheme [aVar] [] (fun (listOf a) (listOf a)))
  , (builtinName "null", scheme [aVar] [] (fun (listOf a) boolTy))
  , (builtinName "length", scheme [aVar] [] (fun (listOf a) intTy))
  , (builtinName "++", scheme [aVar] [] (fun (listOf a) (fun (listOf a) (listOf a))))
  , (builtinName "take", scheme [aVar] [] (fun intTy (fun (listOf a) (listOf a))))
  , (builtinName "drop", scheme [aVar] [] (fun intTy (fun (listOf a) (listOf a))))
  , (builtinName "reverse", scheme [aVar] [] (fun (listOf a) (listOf a)))
  , (builtinName "concat", scheme [aVar] [] (fun (listOf (listOf a)) (listOf a)))
  , (builtinName "concatMap", scheme [aVar, bVar] [] (fun (fun a (listOf b)) (fun (listOf a) (listOf b))))
  , (builtinName "zip", scheme [aVar, bVar] [] (fun (listOf a) (fun (listOf b) (listOf (tupleOf2 a b)))))
  , (builtinName "fst", scheme [aVar, bVar] [] (fun (tupleOf2 a b) a))
  , (builtinName "snd", scheme [aVar, bVar] [] (fun (tupleOf2 a b) b))
  , (builtinName "error", scheme [aVar] [] (fun stringTy a))
  , (builtinName "undefined", scheme [aVar] [] a)
  , (builtinName "show", scheme [aVar] [] (fun a stringTy))
  , (builtinName "succ", scheme [] [] (fun intTy intTy))
  , (builtinName "pred", scheme [] [] (fun intTy intTy))
  , (builtinName "iszero", scheme [] [] (fun intTy boolTy))
  , (builtinName "add", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "negate", scheme [] [] (fun intTy intTy))
  , (builtinName "div", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "mod", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "quot", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "rem", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "even", scheme [] [] (fun intTy boolTy))
  , (builtinName "odd", scheme [] [] (fun intTy boolTy))
  , (builtinName "enumFrom", scheme [] [] (fun intTy (listOf intTy)))
  , (builtinName "enumFromThen", scheme [] [] (fun intTy (fun intTy (listOf intTy))))
  , (builtinName "enumFromTo", scheme [] [] (fun intTy (fun intTy (listOf intTy))))
  , (builtinName "enumFromThenTo", scheme [] [] (fun intTy (fun intTy (fun intTy (listOf intTy)))))
  , (builtinName "seq", scheme [aVar, bVar] [] (fun a (fun b b)))
  , (builtinName "fix", scheme [aVar] [] (fun (fun a a) a))
  , (builtinName "+", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "-", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "*", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "/", scheme [] [] (fun intTy (fun intTy intTy)))
  , (builtinName "==", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName "/=", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName "<", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName "<=", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName ">", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName ">=", scheme [] [] (fun intTy (fun intTy boolTy)))
  , (builtinName "&&", scheme [] [] (fun boolTy (fun boolTy boolTy)))
  , (builtinName "||", scheme [] [] (fun boolTy (fun boolTy boolTy)))
  , (builtinName ".", scheme [aVar, bVar, cVar] [] (fun (fun b c) (fun (fun a b) (fun a c))))
  , (builtinName "$", scheme [aVar, bVar] [] (fun (fun a b) (fun a b)))
  , (builtinName "print", scheme [aVar] [] (fun a (ioOf unitTy)))
  , (builtinName "putStr", scheme [] [] (fun stringTy (ioOf unitTy)))
  , (builtinName "putStrLn", scheme [] [] (fun stringTy (ioOf unitTy)))
  , (builtinName "getLine", scheme [] [] (ioOf stringTy))
  , (builtinName "return", scheme [aVar] [] (fun a (ioOf a)))
  , (builtinName ">>=", scheme [aVar, bVar] [] (fun (ioOf a) (fun (fun a (ioOf b)) (ioOf b))))
  , (builtinName ">>", scheme [aVar, bVar] [] (fun (ioOf a) (fun (ioOf b) (ioOf b))))
  , (builtinName "fail", scheme [aVar] [] (fun stringTy (ioOf a)))
  ]
  where
    aVar = tvar (-3001) "a"
    bVar = tvar (-3002) "b"
    cVar = tvar (-3003) "c"
    a = TVariable aVar
    b = TVariable bVar
    c = TVariable cVar
