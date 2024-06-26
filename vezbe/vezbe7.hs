{-
    Grupisati uzastopne elemente liste u podliste koje sadrze samo te elemente.
    ['a','a','a','b','c','c'] -> [['a','a','a'], ['b'], ['c','c']]
-}

str = "aaabaabbbcbbbcccdcccddd"

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = (x : takeWhile (== x) xs) : (pack . dropWhile (== x) $ xs)

{-
    Grupisati uzastopne elemente liste u tuple-ove (<element>, <broj_ponavljanja> :: Int).
-}

pack' :: (Eq a) => [a] -> [(a, Int)]
pack' [] = []
pack' xxs = [(head xs, length xs) | xs <- pack xxs, length xs > 0]

{-
    Definisati tip koji predstavlja tupleove iz prethodnog zadatka tako da razlikuje ako se elemet
    ponavlja vise puta ili samo jednom. Zatim prepraviti funkciju iz prethodnog zadatka da vraca listu
    elemenata navedenog tipa.
-}

data MyPacked a = Single a | Multi Int a

instance (Eq a) => Eq (MyPacked a) where
    (Single x) == (Single y) = x == y
    (Multi i x) == (Multi j y) = (i == j) && (x == y)
    _ == _ = False

pack'' :: (Eq a) => [a] -> [MyPacked a]
pack'' [] = []
pack'' xs = [if i > 1 then Multi i x else Single x | (x, i) <- pack' xs]

{-
    Napisati funkciju koja pretvara rezultat prethodnog zadatka nazad u listu.
-}

decode :: MyPacked a -> [a]
decode (Single x) = [x]
decode (Multi i x) = replicate i x  -- take i $ repeat x

unpack :: [MyPacked a] -> [a]
unpack [] = []
unpack (x:xs) = decode x ++ unpack xs

unpack' :: [MyPacked a] -> [a]
unpack' = foldr ((++) . decode) []

{-
    Napisati instancu klase show za tip iz prethodnog zadatka da prikazuje podatke u citljivom obliku.
-}

instance (Show a) => Show (MyPacked a) where
    show (Single x) = show x
    show (Multi i x) = show x ++ "x" ++ show i

{-
    Definisati tip i funkcije (insert i elem) za binary search tree. 
-}

data Stablo a = Prazno | Stablo a (Stablo a) (Stablo a) deriving (Show)

dodaj :: (Ord a) => a -> Stablo a -> Stablo a
dodaj x Prazno = Stablo x Prazno Prazno
dodaj x (Stablo y levo desno)
        | x == y = Stablo y levo desno
        | x < y = Stablo y (dodaj x levo) desno
        | x > y = Stablo y levo (dodaj x desno)

nadji :: (Ord a) => a -> Stablo a -> Bool
nadji x Prazno = False
nadji x (Stablo y levo desno)
        | x == y = True
        | x < y = nadji x levo
        | x > y = nadji x desno

{-
    Napraviti funkcije koje pretvaraju listu u stablo i stablo u listu.
-}

fromList :: (Ord a) => [a] -> Stablo a
fromList xs = foldr dodaj Prazno xs

toList :: Stablo a -> [a]
toList Prazno = []
toList (Stablo x levo desno) = x : ((toList levo) ++ (toList desno))

{-
    Napisati funkciju koja rotira triple za zadati broj mesta u levo.
    Broj mesta se zadaje tipom koji moze imati 3 vrednosti.
-}

data Broj = Nula | Jedan | Dva

rotate :: Broj -> (a, a, a) -> (a, a, a)
rotate Nula (x, y, z) = (x, y, z)
rotate Jedan (x, y, z) = (y, z, x)
rotate Dva (x, y, z) = (z, x, y)

{-
    Napisati funkciju koja reci u recenici ispisuje u nazad, a ostavlja ih na istom mestu u recenici.
-}

uNazad :: String -> String
uNazad "" = ""
uNazad str = unwords . map reverse . words $ str

{-
    Napisati program koji sa standardnog ulaza ucitava recenice i ispisuje ih u nazad kao u prethodnom zadatku.
    Osim u slucaju da je recenica palindrom. Tada ispisuje "Palindrom".
-}

palindrom :: String -> Bool
palindrom str = str == reverse str

removeSpaces :: String -> String
removeSpaces str = foldl (\s acc -> s ++ acc) "" . words $ str

printLine :: String -> IO ()
printLine "" = putStrLn ""
printLine str
    | palindrom . removeSpaces $ str = putStrLn "Palindorm"
    | otherwise = putStrLn . uNazad $ str

main = do line <- getLine
          if null line
            then return ()
            else do printLine line
                    main

{-

    import Control.Monad(when)
    
    main = do line <- getLine
              when (not . null $ line) $ do printLine line
                                            main

-}

{-
    Napisati tip Imenik koji sadrzi ime, broj telefona i e-mail.
-}

data Imenik = Imenik [Osoba]
data Osoba = Osoba {ime :: String, telefon :: String, mail :: String}

{-
    Napisati funkciju koja za zadati Imenik vraca list tuple-ova koji sadrze samo ime i broj telefona.
-}

noMail :: Imenik -> [(String, String)]
noMail (Imenik []) = []
noMail (Imenik (osoba:osobe)) = (ime osoba, telefon osoba) : noMail (Imenik osobe)