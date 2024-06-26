{-  Tipovi

    Type signature
        - addThree :: Int -> Int -> Int -> Int
    Type annotation
        - x = 3 :: Int
    
    Type variable
        - polimorfne funkcije
        - head :: [a] -> a
        - fst :: (a, b) -> a
        - map :: (a -> b) -> [a] -> [b]

-}

{-  Klase tipova (Typeclass)

    Najpribliznije interfejsima kod OOP
    
    Class constraint
        - (==) :: (Eq a) => a -> a -> Bool
    
    Primeri
        - Eq - jednakost
        - Ord - poredjenje
        - Show - mogu da se predstave kao string
        - Read - mogu da se procitaju iz stringa
        - Enum - nabrojivi
        - Num - numericki
        - Integral - celobrojni
        - Floating - realni brojevi
    
    Ne postoji polimorfizam kao u OOP
        - postoji tipski polimorfizam

-}

{-  Funkcije

    Pattern matching
        - vise tela funkcije
        - poziva se ona ciji pattern odgovara prosledjenim podacima
        - "_" se koristi za podatak koji nam nije vazan
        - as patterns "xs@(x:y:ys)" - cuva referencu do cele liste

-}

-- primer (pattern) test (tipovi, typeclass Num, Int), vector, factorial, head' (error), prazna lst, len', sum'

manje :: (Ord a) => a ->  a -> Bool
manje x y = x < y

test :: (Num a, Eq a) => a -> String
test 7 = "Sedam"
test x = "Nije sedam"

factorial :: (Integral a) => a -> a
factorial 0 = 1  --edge condition
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x 
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
-- tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
tell xs@(x:y:ys) = "Prvi: " ++ show x ++ ", drugi: " ++ show y ++ ", ostali: " ++ show ys ++ ", svi: " ++ show xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs -- sum' (x:xs) = x + sum' xs
