data Person = Person { name :: [Char], age :: Int }

birthday (Person n a) = Person n (a + 1)

main = name (birthday (Person "Ada" 36))
