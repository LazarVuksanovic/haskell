{-  Jos malo funkcija

    Guard
        - slicno kao pattern ali ne proverava jednakost nego zadajemo Bool izraz
        - otherwise - ako svi prethodni vrate False (slicno kao "_")
        - rade u kombinaciji sa patternima (proveravaju se redom kojim su navedeni)
    
    Where
        - binduje novo ime unutar guardova
            - npr. ako se neko izracunavanje ponavlja u vise guardova
        - povecava citljivost i performanse
        - imena se ne dele izmedju tela funkcije (pattern)
        - imena mogu biti konstante ali mogu i funkcije
        - mogu biti ugnjezdene
        - mozemo ih pisati inline razdvojene sa ";"
    
    Let
        - let <bindings> in <expression>
        - slicno kao where ali su imena vidljiva samo u in izrazu
        - ponasaju se kao izraz pa mogu da se koriste bilo gde
        - mozemo ih pisati inline razdvojene sa ";"
        - ako ih koristimo kao predikat u list comprehensionu imena su vidljiva izlaznoj funkciji
            - ako stavimo "in" onda su vidljiva samo tom predikatu
    
    Case
        - case expression of
                   pattern -> result
                   pattern -> result
                   pattern
                      | guadr -> result
                      | guard -> result
                   ...
                   otherwise -> result
        - takodje izraz i moze se koristiti bilo gde
        - slicno kao pattern matching i guard
    
    Rekurzija
        - "Recursion is important to Haskell because unlike imperative languages, you do computations
           in Haskell by declaring what something is instead of declaring how you get it."
        - granicni slucajevi odredjuju kada se rekurzija prekida
            - validno je definisati beskonacnu rekurzivnu funkciju

-}

-- primer (guard) test, cmp (infix, Ord)

test' :: Int -> String
test' a
    | a == 1    = "jedan"
    | a == 2    = "dva"
    | otherwise = "nesto"

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- primer (where) inicijali (pattern match)

test'' :: Int -> Int -> String
test'' x y
        | zbir < broj = "Manje"
        | zbir == broj = "Jednako"
        | zbir > broj = "Vece"
        where zbir = x + y  -- zbir x y = x + y
              broj = 5

initials :: String -> String -> String
initials firstname lastname = ime ++ ": " ++ [f] ++ ". " ++ [l] ++ "."
    where ime = firstname ++ " " ++ lastname
          (f:_) = firstname
          (l:_) = lastname

-- primer (let) fn, izraz, pattern match

cylinder :: Float -> Float -> Float
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

primer :: (Num a, Ord a) => [(a, a)] -> [a]
primer xs = [a + b | x <- xs, let (a,b) = x]
-- primer xs = [fst x + snd x | x <- xs, let (a,b) = x in a + b > 3]

-- primer (case)

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- primer (recusion)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)  -- maximum'' (x:xs) = max x (maximum' xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
    
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted