module Kolokvijum2G2 where

import GHC.Base (undefined)
import Text.Parsec
import Data.List
import Text.Read
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception (catch)

-- 1. Definisati tip Stack koji ima 2 vrednosti Stack i Prazan. Stack sadrzi vrednosti proizvoljnog tipa.
-- Velicina Stacka nije ogranicena.

data Stack a = Stack [a] | Prazan

--   Zatim instancirati klasu Eq i klasu Show tako da prikazuje Stack kao sto je prikazano u primerima.

--      Primeri:
--         show (Stack [1,2,3]) => "***
--                                  3
--                                  2
--                                  1
--                                  ***"
--         show (Stack ["pera","mika","laza"]) => "***
--                                                 \"laza\"
--                                                 \"mika\"
--                                                 \"pera\"
--                                                 ***"
--         show Prazan => "Prazan"

-- 2. Instancirati klasu Functor za tip Stack iz 1. zadatka.


instance (Show a) => Show (Stack a) where
    show Prazan = "Prazan"
    show (Stack xs) = "***\n" ++ (concatMap (\x -> (show x) ++ "\n") xs) ++ "***"


instance (Eq a) => Eq (Stack a) where
   (Stack xs) == (Stack ys) = xs == ys


instance Functor Stack where
    fmap f (Stack xs) = Stack (fmap f xs)


-- 3. Instancirati klasu Applicative za tip Stack iz 1. zadatka.
-- U slucaju da levi operand sadrzi samo jednu funkciju treba je primeniti na sve elemente desnog operanda.
-- U slucaju da levi operand sadrzi veci broj funkcija treba primeniti prvu funkciju na prvi element desnog operanda,
-- drugu na drugi itd. Ako nije isti broj funkcija i dimenzija vektora treba vratiti Prazan.

--  Primeri:
-- pure 5 :: Stack Int => "***
--                         5
--                         ***"
-- (+) <$> Stack [1,2,3] <*> Stack [4,5,6] => "***
--                                             9
--                                             7
--                                             5
--                                             ***"
-- (+) <$> Stack [1,2,3,4] <*> Stack [4,5,6] => Prazan
-- (+) <$> Prazan <*> Stack [4,5,6] => Prazan


instance Applicative Stack where
    pure x = Stack [x]
    Prazan <*> _ = Prazan
    _ <*> Prazan = Prazan
    (Stack fs) <*> (Stack xs)
        | length fs == 1 = Stack (fmap (head fs) xs)
        | length fs == length xs = Stack [f x | (f, x) <- (zip fs xs)]
        | otherwise = Prazan


-- 4. Instancirati klase Semigroup i Monoid za tip Stack iz 1. zadatka.
--   Instanca treba da radi i sa Prazan i sa praznom listom.


instance Semigroup (Stack a) where
    Prazan <> st = st
    st <> Prazan = st
    (Stack xs) <> (Stack ys) = Stack (xs `mappend` ys)


instance Monoid (Stack a) where
    mempty = Prazan

-- 5. Instancirati klasu Monad za tip Stack iz 1. zadatka.
--    Obratiti paznju da monadic funkcija ima tip (a -> Stack a), ne ([a] -> Stack a).


instance Monad Stack where
    Prazan >>= _ = Prazan
    Stack xs >>= f = foldl mappend Prazan (fmap f xs)


-- Primeri:
--         return 5 :: Stack Int => "***
--                                   5
--                                   ***"
--         (Stack [1,2,3]) >>= return => "***
--                                        3
--                                        2
--                                        1
--                                        ***"
--         (Stack []) >>= return => Prazan

{- 6.
	Napisati sledece funkcije. Funkcija makeStack uzima listu Stringova ["1", "12", "3"] i vraća Stack sa Int vrednostima.
    Smatrati da je ulaz ispravan.

    Funkcija push uzima vrednost i Stack i dodaje tu vrednost na pocetak Stacka.
    Funkcija pop uklanja element sa pocetka Stacka. Ako je rezultujuci Stack prazan treba da vrati
    vrednost Prazan. Ukoliko se pozove sa praznim stackom daje exception. Funkcija vraca tuple gde je
    prvi element vrednost uklonjena sa Stacka, a druga Stack sa preostalim vrednostima.
    Napomena: Obratiti paznju da prazan Stack moze biti predstaljen sa 2 vrednosti.
-}

makeStack :: [String] -> Stack Int
makeStack ss = Stack (reverse (map read ss))

push :: a -> Stack a -> Stack a
push a (Stack xs) = Stack (a : xs)
push a Prazan = Stack [a]


pop :: Stack a -> (a, Stack a)
pop Prazan = error "Cannot pop from an empty stack"
pop (Stack []) = error "Cannot pop from an empty stack"
pop (Stack (x:xs))
    | length xs == 0 = (x, Prazan)
    | otherwise = (x, Stack xs) 



{- 7.
	Napisati parser za rad sa Stackom. Ulaz parsera je linija sa pocetnim stanjem Stacka i nizom operacija.
	Izlaz je stanje Stacka nakon izvrsavanja svih operacija. Parser treba da podrži učitavanje samo Stacka,
    bez operacija. Ukoliko se ne unese Stack na pocetku ulaza smatrati da je pocetni Stack prazan. Treba podrzati
    operacije "push" i "pop" iz predhodnog zadatka. Oblik unosa je dat u primerima. Smatrati da je unos isprvan.
    Manjak ili visak praznih mesta se ne racuna u gresku.

	Napomena:
		Cuvanje trenutne vrednosti u toku izracunavanja u "stanju" parsera moze da olaksa rad.
    Primeri:
        Stack [1,2,3] => "***
                          3
                          2
                          1
                          ***"
        Stack [1,2,3], pop => "***
                               3
                               2
                               ***"
        Stack [1,2,3], push 4 => "***
                                  3
                                  2
                                  1
                                  4
                                  ***"
         Stack  [ 1,  2, 3 ] ,  push4   => "***
                                            3
                                            2
                                            1
                                            4
                                            ***"
        Stack [1,2,3], pop, push 4 => "***
                                       3
                                       2
                                       4
                                       ***"
        push 4 => "***
                   4
                   ***"
        Stack [1], pop => Prazan
-}

-- parser :: Parsec String (Stack Int) (Stack Int)

number :: Parsec String (Stack Int) (String)
number = do many1 digit

minus :: Parsec String (Stack Int) (String)
minus = (:) <$> char '-' <*> (spaces >> number)

values :: Parsec String (Stack Int) [String]
values = do sepBy1 (minus <|> number) (spaces >> char ',' >> spaces)

stack :: Parsec String (Stack Int) ()
stack = do spaces
           string "Stack"
           spaces
           vals <- between (spaces >> char '[' >> spaces) (spaces >> char ']' >> spaces) values
           char ','
           spaces
           putState (makeStack vals)
           return ()

onlyStack :: Parsec String (Stack Int) ()
onlyStack = do spaces
               string "Stack"
               spaces
               vals <- between (spaces >> char '[' >> spaces) (spaces >> char ']' >> spaces) values
               eof
               putState (makeStack vals)
               return ()

operations :: Parsec String (Stack Int) ()
operations = do try parsePop <|> parsePush <|> try stack
                return ()

parsePop :: Parsec String (Stack Int) ()
parsePop = do spaces
              string "pop"
              s <- getState
              let (_, ss) = pop s
              putState ss
              return ()

parsePush :: Parsec String (Stack Int) ()
parsePush = do spaces
               string "push"
               spaces
               n <- number
               s <- getState
               putState (push (read n) s)
               return ()

stackOperations :: Parsec String (Stack Int) ()
stackOperations = do spaces
                     sepBy1 operations (spaces >> char ',' >> spaces)
                     return ()
                     

start :: Parsec String (Stack Int) (Stack Int)
start = do try onlyStack <|> stackOperations
           s <- getState
           return (s)
-- 8.
-- 	Napisati program koji unos sa konzole parsira parserom iz 7. zadatka i na konzoli prikazuje rezultat parsiranja.
-- 	Program prekida izvrsavanje kada se unese prazna linija.

main :: IO ()
main = do line <- getLine
          if null line then return ()
                       else do case runParser start Prazan "Console" line of Right x -> print x
                                                                             Left err -> print err
                               main

                               main :: IO ()
-- main = catch probaj handler'

probaj :: IO ()
probaj = do file <- getLine
            cont <- readFile file
            case runParser start Prazan "" cont of Right x -> print x
                                                   Left err -> print err

handler' :: IOError -> IO ()
handler' e
    | isDoesNotExistError e = putStrLn "Ne postoji"
    | otherwise = ioError e