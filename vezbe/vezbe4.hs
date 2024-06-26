{-  Funkcije viseg reda

    Imaju funkciju kao parametar ili povratnu vrednost
    
    Curried funkcije
        - parcijalno primenjene funkcije
            - pozvane sa manje argumenata
        - section
            - infiksne funkcije pozvane sa manje argumenata
            - pisu se u zagradama
            - nije bitno koji argument fali

    Lambda funkcije
        - \<lista parametara> -> <telo funkcije>
    
    Fold
        - vracaju akumuliranu vrednost
        - foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
        - foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
        - foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
        - foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
        - scanl :: (b -> a -> b) -> b -> [a] -> [b]
        - scanr :: (a -> b -> b) -> b -> [a] -> [b]
        - scanl1 :: (a -> a -> a) -> [a] -> [a]
        - scanr1 :: (a -> a -> a) -> [a] -> [a]
        - desni fold radi sa beskonacnim listama
        - foldl1 i foldr1 ne rade sa praznim listama
        - scan radi isto sto i fold samo prijavljuje medjurezultate izvrsavanja
    
    Funkcija $
        - aplikacija funkcije
        - ($) :: (a -> b) -> a -> b
        - ima najnizi prioritet
        - grupise na desno
    
    Kompozicija funkcija
        - (.) :: (b -> c) -> (a -> b) -> a -> c
        - voditi racuna o tipovima funkcija
        - ako funkcija uzima vise parametara mora biti parcijalno primenjena
          tako da uzima samo jedan

-}

-- primer FVR

zbir :: (Num a) => a -> a -> a
zbir x y = x + y

saberi5 :: (Num a) => a -> a
saberi5 = zbir 5

divTen :: (Floating a) => a -> a
divTen = (/10)

pripada :: Char -> Bool
pripada = (`elem` ['A'..'Z'])  -- (1 `elem`)

doMe :: (a -> a) -> a -> a
doMe f x = f x  -- doMe2

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zip'

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

deljiv :: (Integral a) => a -> a
deljiv x = head (filter p [100000,99999..])
    where p y = y `mod` x == 0

-- primer lambda

-- zipWith' (\a b -> a + b) [5,4,3,2,1] [1,2,3,4,5]
-- zipWith' (\a -> \b -> a + b) [5,4,3,2,1] [1,2,3,4,5]
-- map' (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- primer fold

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs  -- scanl
-- sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [[b]]
map'' f xs = scanr (\x acc -> f x : acc) [] xs  -- scanr

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sqrtSums' :: Int
sqrtSums' = (+) 1 . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]

{-  Moduli

    Sadrze definicije funkcija i tipova itd.
    
    Osnovni modul je Prelude
        - po defaultu je includeovan
    
    Sintaksa
        - import <module.name>
        - import <module.name> (imena, koja, importujemo)
        - import <module.name> hiding (imena, koja, ne, importujemo)
        - import qualified <module.name>
        - import qualified <module.name> as ime
    
    Lista modula u standardnoj biblioteci
        - https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
    
    Neki zanimljivi moduli
        - Data.List
        - Data.Char
        - Data.Map
        - Data.Set

-}

{-  Domaci

    - Vigenere cipher
        - Uzima kljuc(string) i tekst i vraca enkriptovan tekst
        - Uzima kljuc(string) i tekst i vraca dekriptovan tekst
    
    - Napisati f-ju koja uzima lisu parcijalnih funkcija i listu parametara
      i vraca listu koja sadrzi rezultate primene svake funkcije na svaki argument
      
    - Isto kao u prethodnom zadatku ali vraca prvu funkciju primenjenu na prvi argument,
      drugu na drugi itd.
    
    - Prepraviti prethodne 2 funkcije da vracaju akumuliran rezultat
    
    - Napisati funkciju koja proverava da li je uneti string palindrom 
    
    - Napisati funkciju koja uzima broj kao parametar i pronalazi sve proste brojeve
      manje od tog broja
    
    - Napisati funkciju koja vraca pretposlednji element liste
    
    - Napisati funkciju koja racuna srednju vrednost i standardnu devijaciju unete liste
-}