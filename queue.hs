import Text.Parsec
import Text.Read
import Data.List
import Control.Exception (catch)
import System.Environment
import System.IO
import System.IO.Error
import Control.Monad

-- 1. [3]
-- Definisati tip Queue (FIFO red) koji ima 2 vrednosti Queue i Prazan. Queue sadrzi vrednosti proizvoljnog tipa.
-- Duzina reda nije ogranicena.
-- Zatim instancirati klasu Eq i klasu Show tako da prikazuje redove kao sto je prikazano u primerima.
-- Primeri:
--     show (Queue [1,2,3]) => "|1,2,3>"
--     show (Queue ["pera","mika","laza"]) => "|\"pera\",\"mika\",\"laza\">"
--     show Prazan => "Prazan"

data Queue a = Queue [a] | Prazan

instance Show a => Show (Queue a) where
    show Prazan = "Prazan"
    show (Queue xs) = "|" ++ concatMap (\x -> show x ++ ", ") xs ++ ">"

instance Eq a => Eq (Queue a) where
    Prazan == Prazan = True
    Prazan == (Queue xs) = False
    (Queue xs) == (Queue ys) = xs == ys


{- 2. [1]

	Instancirati klasu Functor za tip Queue iz 1. zadatka.

    Primeri:
        fmap (+1) (Queue [1,2,3]) => |2,3,4>
        fmap (++"!") (Queue ["pera","mika","laza"]) => |"pera!","mika!","laza!">
-}

instance Functor Queue where
    fmap f (Queue xs) = Queue (fmap f xs)

{- 3. [3]
	Instancirati klasu Applicative za tip Queue iz 1. zadatka.
	U slucaju da levi operand sadrzi samo jednu funkciju treba je primeniti na sve elemente desnog operanda.
	U slucaju da levi operand sadrzi veci broj funkcija treba primeniti prvu funkciju na prvi element desnog operanda,
	drugu na drugi itd. Ako nije isti broj funkcija i dimenzija vektora treba vratiti Prazan.

    Primeri:
        pure 5 :: Queue Int => <5>
        (+) <$> Queue [1,2,3] <*> Queue [4,5,6] => |5,7,9>
        (+) <$> Queue [1,2,3,4] <*> Queue [4,5,6] => Prazan
        (+) <$> Prazan <*> Queue [4,5,6] => Prazan
-}

instance Applicative Queue where
    pure x = Queue [x]
    Prazan <*> _ = Prazan
    _ <*> Prazan = Prazan
    (Queue fs) <*> (Queue xs)
        | length fs == 1 = Queue (fmap (head fs) xs)
        | length fs == length xs = Queue [f x | (f, x) <- zip fs xs]
        | otherwise = Prazan

{- 4. [2]

	Instancirati klase Semigroup i Monoid za tip Queue iz 1. zadatka.

	Napomena:
		Instanca treba da radi i sa Prazan i sa praznom listom.

-}

instance Semigroup (Queue a) where
    Prazan <> st = st
    st <> Prazan = st
    (Queue xs) <> (Queue ys) = Queue (xs `mappend` ys)

instance  Monoid (Queue a) where
    mempty = Prazan

{- 5. [2]

   	Instancirati klasu Monad za tip Queue iz 1. zadatka.

	Napomena:
		Obratiti paznju da monadic funkcija ima tip (a -> Queue a), ne ([a] -> Queue a).

    Primeri:
        return 5 :: Queue Int => |5>
        (Queue [1,2,3]) >>= return => |1,2,3>
        (Queue []) >>= return => Prazan
		(Queue [1,2]) >>= \x->Queue[(x+1),(x+2)] => "|2,3,3,4>"

-}

instance Monad Queue where
    Prazan >>= _ = Prazan
    (Queue xs) >>= f = foldl mappend Prazan (fmap f xs)


{- 6. [1]

    Napisati sledece funkcije. Funkcija makeQueue uzima listu Stringova ["1", "12", "3"] i vraća Queue sa Int vrednostima.
    Smatrati da je ulaz ispravan.

    Funkcija push uzima vrednost i Queue i dodaje tu vrednost na pocetak Queuea.
    Funkcija pop uklanja element sa kraja Queuea. Ako je rezultujuci Queue prazan treba da vrati
    vrednost Prazan. Ukoliko se pozove sa praznim Queueom daje exception. Funkcija vraca tuple gde je
    prvi element vrednost uklonjena iz Queuea, a druga Queue sa preostalim vrednostima.
    Napomena: Obratiti paznju da prazan Queue moze biti predstaljen sa 2 vrednosti.

-}

makeQueue :: [String] -> Queue Int
makeQueue ss = Queue (map read ss)

push :: a -> Queue a -> Queue a
push a Prazan = Queue [a]
push a (Queue xs) = Queue (a : xs)

pop :: Queue a -> (a, Queue a)
pop Prazan = error "Prazan Queue!"
pop (Queue []) = error "Prazan Queue!"
pop (Queue xs)
    | length xs == 1 = (head xs, Prazan)
    | otherwise = (head r, Queue (reverse (tail r)))
        where r = reverse xs

{- 7. [10]

	Napisati parser za rad sa Queueom. Ulaz parsera je linija sa pocetnim stanjem Queuea i nizom operacija.
	Izlaz je stanje Queuea nakon izvrsavanja svih operacija. Parser treba da podrži učitavanje samo Queuea,
    bez operacija. Ukoliko se ne unese Queue na pocetku ulaza smatrati da je pocetni Queue prazan. Treba podrzati
    operacije "push" i "pop" iz predhodnog zadatka. Oblik unosa je dat u primerima. Smatrati da je unos isprvan.
    Manjak ili visak praznih mesta se ne racuna u gresku.

	Napomena:
		Cuvanje trenutne vrednosti u toku izracunavanja u "stanju" parsera moze da olaksa rad.
    Primeri:
        Queue [1,2,3] => "|1,2,3>"
        Queue [1,2,3], pop => "|1,2>"
        Queue [1,2,3], push 4 => "|4,1,2,3>"
        Queue  [ 1,  2, 3 ] ,  push4   => "|4,1,2,3>"
        Queue [1,2,3], pop, push 4 => "|4,1,2>"
        push 4 => "|4>"
        Queue [1], pop => Prazan

-}

number :: Parsec String (Queue Int) String
number = many1 digit

minus :: Parsec String (Queue Int) String
minus = (:) <$> char '-' <*> (spaces >> number)

values :: Parsec String (Queue Int) [String]
values = sepBy1 (minus <|> number) (spaces >> char ',' >> spaces)

queue :: Parsec String (Queue Int) ()
queue = do spaces
           string "Queue"
           spaces
           vals <- between (spaces >> char '[' >> spaces) (spaces >> char ']' >> spaces) values
           char ','
           spaces
           putState (makeQueue vals)
           return ()

singleQueue :: Parsec String (Queue Int) ()
singleQueue = do spaces
                 string "Queue"
                 spaces
                 vals <- between (spaces >> char '[' >> spaces) (spaces >> char ']' >> spaces) values
                 eof
                 putState (makeQueue vals)
                 return ()

parsePop :: Parsec String (Queue Int) ()
parsePop = do string "pop"
              s <- getState
              let (_, ss) = pop s
              putState ss

parsePush :: Parsec String (Queue Int) ()
parsePush = do spaces
               string "push"
               spaces
               n <- number
               s <- getState
               putState (push (read n) s)

parseSinglePush :: Parsec String (Queue Int) ()
parseSinglePush = do spaces
                     string "push"
                     spaces
                     n <- number
                     s <- getState
                     eof
                     putState (push (read n) s)

queueOperations :: Parsec String (Queue Int) ()
queueOperations = do q <- queue
                     sepBy1 (try parsePop <|> try parsePush) (spaces >> char ',' >> spaces)
                     return ()

start :: Parsec String (Queue Int) (Queue Int)
start = do try singleQueue <|> try queueOperations
           getState

{- 8. [3]
	Napisati program koji unos sa konzole parsira parserom iz 7. zadatka i na konzoli prikazuje rezultat parsiranja.
	Program prekida izvrsavanje kada se unese prazna linija.
-}

main = do line <- getLine
          if null line then return ()
                       else do case runParser start Prazan "Console" line of Right x -> print x
                                                                             Left e -> print e
                               main

-- main = catch probaj handler'

probaj :: IO ()
probaj = do file <- getLine
            cont <- readFile file
            case runParser start Prazan "" cont of Right x -> print x
                                                   Left err -> print err


handler' :: IOError -> IO ()
handler' e
    | isDoesNotExistError e = putStrLn "Nema!"
    | otherwise = ioError e