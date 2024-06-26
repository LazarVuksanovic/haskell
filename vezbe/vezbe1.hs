{-  Uvod

    Igor Ciganovic - iciganovic@raf.rs
    
    Konsultacije
        - Zakazuju se mailom
    
    Nacin polaganja
        - 2 kolokvijuma (na racunarima)
			- Haskell, 1. kolokvijumska nedelja, 20 bodova
			- Haskell, 2. kolokvijumska nedelja, 25 bodova
		- test (na papiru)
			- Teorija (zadaci), 5. ili 6. nedelja u terminu predavanja, 15 bodova
        - projekat
            - usmena odbrana, u ispitnom roku, 40 bodova
        - bonus 5+5 bodova
    
    Literatura
        - http://learnyouahaskell.com/  <-  PREDAVANJA
        - http://book.realworldhaskell.org/
             
    Instalacija
        - https://www.haskell.org
        - Notepad++ (Preferences -> Language -> Tab Settings -> Haskell -> Replace by space (4))
		- VS Code + Haskell extension
        - https://github.com/Idokorro/haskell
    
    Glasgow Haskell Compiler
        - ghc - compiler
        - [win|]ghci - interaktivno okruzenje
        - runghc - pokretanje bez kompajliranja
    
    Cabal packaging system (pip u Pythonu)
        - cabal install mypackage
        - repozitorijum: http://hackage.haskell.org/
    
-}

{-  Haskell

    Cisto funkcionalni jezik
        - ne postoji stanje
        - promenljive su konstante
        - funkcije su objekti prvog reda
        - nema sporednih efekata
    
    Lazy
        - sve pakuje u thunk-ove
        - moze da radi sa beskonacnim podacima
        - moze da zauzme vise memorije
    
    Statically typed
        - podrzava type inference
        - nema implicitnog kastovanja
    
-}

{-  WinGHCi

    Interaktivno okruzenje (kao Python)
    
    Sve komande pocinju sa ":"
        - :? - help
        - :load <file> - ucitava <file> u okruzenje
        - :reload - ponovo ucitava sve ucitane fajlove
        - :script <file.hs> - izvrsava <file>
        - :module <mod> - Ucitava modul
            - Prelude je osnovna biblioteka (modul)
        - :set prompt <sta_god> - menja prompt
            - default pokazuje sve ucitane module
        - :unset promp - vraca na default
        - :!<cmd> - izvrsava sistemsku komandu
        - :[set|unset] +s - podaci o izvrsavanju
        - :[set|unset] +t - podaci o tipu poslednjeg izraza
        - :info <ime> - podaci o navedenom imenu
        - :type <izraz> - tip vrednosti izraza
    
-}

{-  Osnovne operacije i tipovi

    Komentari
        - blok komentar "{- komentar -}"
        - inline komentar "-- komentar"
    
    Arimeticke operacije
        - "+", "-" itd.
            - infixne
            - mogu se pozivati kao prefiksne sa "(+)"
        - negacija
            - "-" prefiksni unarni "operator"
        - "^" - celobrojni stepen
        - "**" - racionalni stepen
    
    Logicke operacije
        - "&&", "||"
            - infixne
            - mogu se pozivati kao prefiksne sa "(&&)"
        - "not" 
            - prefiksni "operator"
    
    Jednakosti
        - "==", "/=", "<=", ">="
            - sve isto kao gore
    
    Strogo tipiziran jezik
        - nema implicitnog kastovanja
        - "1 && True" ne moze
    
    :info <funkcija>
        - fiksnost
        - grupisanje
        - prioritet izvrsavanja
    
    Tipovi
        - Int - ogranicen
        - Integer - neogranicen
        - Int i Integer su razliciti tipovi
        - String - "nesto"
            - "'" je validan karakter
            - stringovi su liste karaktera
        - Char - 'c'
        - ostali standardni tipovi su uglavnom isti kao u drugim jezicima
        - sve ima tip (funkcije, izrazi itd.)
    
    Funkcije
        - "min", "max", "pred", "succ", "odd", "even", "compare" ...
        - "show", "read" - konvertovanje u i iz stringova
        - mogu se pozivati kao infiksne sa "`min`"
        - pozivaju se bez zagrada
        - funkcije imaju najvisi prioritet izvrsavanja
    
    Imena funkcija i promenljivih pocinju malim slovom
    
    Imena Klasa i tipova pocinju velikim slovom

-}

doubleMe x = x + x

doubleUs :: Int -> Int -> (Int, Int)
doubleUs x y = (doubleMe x, doubleMe y)

isSmall :: Int -> String
isSmall x = if x < 5
                then "mali"
                else show 5

{-  Liste

    Lista je tip
        - moze sadrzati elemente koji su drugih tipova
        - svaki "oblik" liste je poseban tip
            - "[]" i "[[]]" su razliciti tipovi
            - duzina nije vazna
            - svi elementi moraju biti istog tipa

    Operacije sa listama
        - "++" - spajanje 2 liste
        - ":" - dodavanje elementa na pocetak
        - "!!" - indeksiranje
    
    Funkcije
        - "head", "last"
        - "tail", "init"
        - "take", "drop"
        - "length", "null"
        - "reverse"
        - "elem"
        - "minimum", "maximum"
        - ...
    
    Liste se mogu porediti
        - porede se leksikografski
    
    Range
        - "[1,3..10]", "['a'..'z']"
        - oprezno sa Float (nisu precizni)
    
    Beskonacne liste
        - "[1..]"
        - "cycle", "repeat", "replicate"
    
    List comprehension
        - [ x | x <- [50..100], x `mod` 7 == 3]

-}

-- primer len, rmnupp, [[]]
len' :: [a] -> Int
len' xs = sum [1 | _ <- xs]

rmNonUpper :: [Char] -> [Char]
rmNonUpper st = [c | c <- st, c `elem` ['A'..'Z']]

even' :: [[Int]] -> [[Int]]
even' xxs = [[x | x <- xs, even x] 
                | xs <- xxs]

{-  Tuple

    Kao liste samo ogranicene duzine
    
    Mogu sadrzati razlicite tipove
    
    Tip im je isti ako su iste duzine i sadrze iste tipove
    
    Tuple, Triple, ...
    
    Funkcije
        - "fst", "snd" - rade samo na parovima
        - "zip" - spaja 2 liste u tuplove

-}

-- primer pitagora

pitagora :: Int -> [(Int,Int,Int)]
pitagora x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2, a < b]

