{-
            FUNKCIONALNO PROGRAMIRANJE
                drugi kolokvijum
    
    IME I PREZIME: Ime Prezime
    BROJ INDEXA: RN br/god
    
    Zadatke resavati u ovom fajlu.
    
    Zadatak se predaje tako sto kliknete na "Source control" tab sa leve strane prozora,
    zatim na "+" dugme desno od "Changes" zaglavlja (stage-ujete sve izmene, ne pojedinacne file-ove).
    Zatim upisete poruku na vrhu kartice (nije bitno sta, samo ne sme da bude prazno), pa kliknete na
    strelicu pored "Commit" dugmeta i izaberete "Commit & Push". Time ste predali zadatak.
    
    Zadatak mozete commit-ovati vise puta u toku vremena kolokvijuma. Nakon isteka vremena bice napravljene kopije
    repozitorijuma i dalje izmene se nece uzimati u obzir prilikom ocenjivanja.
    
    Imena funkcija u zadacima moraju ostati ista radi automatskog testiranja. Mozete dodavati nove funkcije po potrebi.
    
    Ako imate deo koda koji se ne compile-ira (postoji greska), zakomentarisati taj kod prilikom predaje zadatka.
    Ocenjuje se sve sto je napisano, bez obzira da li radi.
    
    U teminalu VS Code-a mozete pokrenutiu "GHCi" kao i testirati zadatke komandom "cabal test prvi" za prvi zadatak,
    "cabal test drugi" za drugi itd.
    
    Kolokvijum traje 2 sata.
    
    Dozvoljena je upotreba knjige i materijala sa vezbi.
    Zadaci se ocenjuju pojedinacno. Ocekivano je da u zadacima koristite resenja iz drugih zadataka.
    Zadaci ce se bodovati kao da su ta resenja ispravna i u slucaju da nisu.
-}

import GHC.Base (undefined)
import Text.Parsec
import Data.List
import Text.Read

{-          *******************    ZADACI    *******************          -}

{- 1.
    Definisati tip Vector koji ima 2 vrednosti Vector i Undefined. Vector sadrži koordinate proizvoljnog tipa.
    Dimenzionalnost vektora nije ograničena.
    Zatim instancirati klasu Show tako da prikazuje vektore kao što je prikazano u primerima i klasu Eq.
    Primeri:
        show (Vector [1,2,3]) => "<1,2,3>"
        show (Vector ["pera","mika","laza"]) => "<\"pera\",\"mika\",\"laza\">"
        show Undefined => "Undefined"
-}

data Vector a = Vector [a] | Undefined

instance (Show a) => Show (Vector a) where
    show Undefined = "Undefined"
    show (Vector xs) = "<" ++ intercalate "," (map show xs) ++ ">"

{- 2.
    
	Instancirati klasu Functor za tip Vector iz 1. zadatka.
	
    Primeri:
        fmap (+1) (Vector [1,2,3]) => <2,3,4>
        fmap (++"!") (Vector ["pera","mika","laza"]) => <"pera!","mika!","laza!">
-}

instance Functor Vector where
    fmap f (Undefined) = Undefined
    fmap f (Vector xs) = Vector (fmap f xs)


{- 3.
	Instancirati klasu Applicative za tip Vector iz 1. zadatka.
	U slučaju da levi operand sadrži samo jednu funkciju treba je primeniti na sve elemente desnog operanda.
	U slučaju da levi operand sadrži veći broj funkcija treba primeniti prvu funkciju na prvi element desnog operanda,
	drugu na drugi itd. Ako nije isti broj funkcija i dimenzija vektora treba vratiti Undefined.
	
    Primeri:
        pure 5 :: Vector Int => <5>
        (+) <$> Vector [1,2,3] <*> Vector [4,5,6] => <5,7,9>
        (+) <$> Vector [1,2,3,4] <*> Vector [4,5,6] => Undefined
        (+) <$> Undefined <*> Vector [4,5,6] => Undefined
-}

primeni :: [a -> b] -> [a] -> [b]
primeni [] _ = []
primeni _ [] = []
primeni (f:fs) (x:xs) = f x : primeni fs xs

instance Applicative Vector where
    pure x = Vector [x]
    Undefined <*> _ = Undefined
    _ <*> Undefined = Undefined
    (Vector fs) <*> (Vector xs)
        | length fs == 1 = Vector (fmap (head fs) xs)
        | length fs == length xs = Vector (primeni fs xs)
        | otherwise = Undefined

{- 4.
    
	Instancirati klase Semigroup i Monoid za tip Vector iz 1. zadatka.
	
	Napomena:
		Instanca treba da radi i sa Undefined i sa praznom listom.
-}

instance Semigroup (Vector a) where
    (Vector xs) <> (Vector ys) =  Vector (xs <> ys)
    (Vector xs) <> Undefined =  (Vector xs)
    Undefined <> (Vector xs) =  (Vector xs)

instance Monoid (Vector a) where
    mempty = Undefined


{- 5.
	Instancirati klasu Monad za tip Vector iz 1. zadatka.
	
	Napomena:
		Obratiti pažnju da monadic funkcija ima tip (a -> Vector a), ne ([a] -> Vector a).
	
    Primeri:
        return 5 :: Vector Int => <5>
        (Vector [1,2,3]) >>= return => <1,2,3>
        (Vector []) >>= return => Undefined
-}

-- dodaj :: Num a => a -> Vector a
-- dodaj x = Vector [(x + 2)]

instance Monad Vector where
    Undefined >>= _ = Undefined
    -- (Vector a) >>= fun = mconcat (map fun a)
    Vector xs >>= f = Vector (concatMap (\(Vector ys) -> ys) (map f xs))
    -- fail _ = Undefined

{- 6.
	Napisati funkciju koja uzima listu Stringova i vraća vektor sa Int koordinatama.
    Smatrati da je ulaz ispravan.
-}

makeVector :: [String] -> Vector Int
makeVector ss = Vector (map read ss)

{- 7.
	Napisati parser za matematičke izraze sa vektorima. Ulaz parsera je linija sa celim izrazom.
	Izlaz je vektor koji se dobije kao rezultat izvršavanja izraza. Parser treba da podrži učitavanje
	samo jednog vektora ili izraze sa 2 ili više vektora, kao što je prikazano u primerima.
	Potrebno je podržati samo "+" i "-" operacije.
	
	Napomena:
		Upotreba instanci iz prethodnih zadataka može da olakša obradu pogrešnog unosa.
		Čuvanje trenutne vrednosti u toku izračunavanja u "stanju" parsera može da olakša rad.
    Primeri:
        <1,2,3> => <1,2,3>
        <1,2,3>+<4,5,6> => <5,7,9>
        <1,2,3>+<4,5,6>-<7,8,9> => <-2,-1,0>
          < 1 , 2,3  >  => <  4,5  ,6 > - <-3,-3,-3>
        <1,2>+<3,4,5> => Undefined
        <asd> => Unexpected ...
        <1,2><3,4> => Unexpected ...
        <1,2>asd => Unexpected ...
-}


minus :: Parsec String (Vector Int) String
minus = (:) <$> char '-' <*> (spaces >> number)

number :: Parsec String (Vector Int) (String)
number = many1 digit


vec :: Parsec String (Vector Int) (Vector Int)
vec = do cnt <- between (spaces >> char '<' >> spaces) (spaces >> char '>' >> spaces) coords
         return (makeVector cnt)
         
coords :: Parsec String (Vector Int) [String]
coords = sepBy1 (minus <|> number) (spaces >> char ',' >> spaces)

operation :: Num a => Parsec String (Vector Int) (a->a->a)
operation = do spaces
               op <- oneOf "+-"
               case op of '+' -> return (+)
                          '-' -> return (-)

singleVec :: Parsec String (Vector Int) (Vector Int)
singleVec = do v <- vec
               eof
               return (v)

eq :: Parsec String (Vector Int) (Vector Int)
eq = do v1 <- vec
        spaces
        op <- operation
        spaces
        v2 <- vec
        return $ op <$> v1 <*> v2

math :: Parsec String (Vector Int) ()
math = do spaces
          op <- operation
          spaces
          v1 <- getState
          v2 <- vec
          putState $ op <$> v1 <*> v2

vectorMath :: Parsec String (Vector Int) (Vector Int)
vectorMath = do first <- vec
                putState first
                many math
                x <- getState
                return (x)
start :: Parsec String (Vector Int) (Vector Int)
start = try (singleVec) <|> vectorMath

main = do input <- getLine
          if null input then return ()
                        else do case runParser vectorMath (Undefined) "Console Input" input of Left err -> print err
                                                                                               Right result -> print result
                                main