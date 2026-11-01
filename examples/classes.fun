class Eq2 a where
  eq2 :: a -> a -> Bool

instance Eq2 Int where
  eq2 x y = x == y

instance Eq2 a => Eq2 [a] where
  eq2 xs ys = case xs of
    [] -> case ys of
            [] -> True
            _  -> False
    (x:xs') -> case ys of
                 [] -> False
                 (y:ys') -> if eq2 x y then eq2 xs' ys' else False

same :: Eq2 a => a -> a -> Bool
same x y = eq2 x y

main = same [1,2,3] [1,2,3]
