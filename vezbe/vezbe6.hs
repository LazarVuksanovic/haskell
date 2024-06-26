{-  Functor

    Type class
        - class Functor (f :: * -> *) where
              fmap :: (a -> b) -> f a -> f b
              (<$) :: a -> f b -> f a
              {-# MINIMAL fmap #-}
    
    Da bi tip pripadao klasi Functor mora implementirati fmap funkciju
    
    Bilo koji tip preko kojeg ima smisla mapiranje moze biti Functor
        - Bilo koji tip koji ima parametar tipa u type konstruktoru
    
    Liste su Functor-i
        - instance Functor [] where  
              fmap = map
    
    Ne navodi se parametar konkretnog tipa
    
    Ako type konstruktor prima 2 parametra tipa mora se parcijalno primeniti

-}

-- primer Functor

data Osoba a = Osoba a | Niko deriving (Show)

instance Functor Osoba where
    fmap f (Osoba x) = Osoba (f x)
    fmap f Niko = Niko

zbir3 :: (Num a) => a -> a
zbir3 x = x + 3

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
    fmap f (Right' x) = Right' (f x)
    fmap f (Left' x) = Left' x

{-  Main

    Kao i u vecini jezika izvrsavanje programa pocinje implicitnim pozivom funkcije main
    
    Obicno se ne pise type signature main funkcije
        - main :: IO ()
    
    Kompajliranje
        - ghc --make <ime_fajla>.hs
        - runhaskell <ime_fajla>.hs
    
    Main funkcija je jedino mesto gde se mogu izvrsavati I/O funkcije
        - I/O funkcije su jedini element Haskell-a koji nije cisto funkcionalan
            - mogu imati sporedne efekte
        - operator "<-" se koristi za citanje vrednosti I/O funkcije
            - svaka I/O funkcija ima povratni tip IO <neki_tip>
            - operator "<-" bind-uje vrednost tipa <neki_tip> u ime sa leve strane
        - vise uzastopnih poziva I/O funkcija mozemo zatvoriti u do blokove
            - blok ima tip povratne vrednosti poslednje pozvane funkcije
            - u svakoj liniji do bloka se moze bind-ovati ime operatorom "<-" osim u poslednjoj
            - bind-ovanje imena koja nisu rezultati I/O funkcija se radi u let izrazu
                - moze se pisati kao u list comprehension-ima bez in bloka
                - ako se pise sa in blokom onda su imena vidljiva samo unutar in bloka
    
    I/O funkcije i blokovi se mogu bind-ovati u proizvoljno ime (funkciju)
        - bice izvrseni tek ako se to ime bind-uje u main
            - I/O funkcije sa proizvoljnim imenom ce biti izvrsene samo unutar main funkcije
    
    Funkcija return radi suprotno od operatora "<-"
        - uzima bilo koji <tip> i vraca IO <tip>
    
    Command line argumenti
        - System.Environment.getArgs :: IO [String]
        - System.Environment.getProgName :: IO String
    
    Korisne funkcije
        - Control.Monad.when :: Applicative f => Bool -> f () -> f ()
        - sequence :: (Monad m, Traversable t) => t (m a) -> m (t a) -- [IO a] -> IO [a]
        - mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
        - mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
        - Control.Monad.forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)

-}

{-  primer main

    main = putStrLn "Hello :)"

    main = do putStrLn "Neki tekst?"
              text <- getLine
              let slova = filter (`elem` ['A'..'Z']) text
              putStrLn $ "Velika slova: " ++ slova

    import Data.Char

    main = do putStrLn "Neki broj?"
              line <- getLine
              if null line
                then return ()
                else do let rez = zbir3 (read line :: Int)
                        putStrLn $ "Rezultat: " ++ show rez
                        main

    main = do a <- return "neko"  
              b <- return "nesto"
              putStrLn $ a ++ " " ++ b

    main = do tekst <- getContents  -- interact kratke
              putStr (kratke tekst)
      
    kratke :: String -> String
    kratke input = let all = lines input
                       short = filter (\line -> length line < 10) all
                       result = unlines short
                   in  map toUpper result

    import System.Environment
    import Data.List

    main = do args <- getArgs
              progName <- getProgName
              mapM putStrLn args
              putStrLn progName

-}

{- Rad sa fajlovima

    Pristup fajlovima
        - openFile :: FilePath -> IOMode -> IO Handle
        - hClose :: Handle -> IO ()
        - withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
    
    Fajlovi otvoreni sa openFile se MORAJU zatvoriti sa hClose
    
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    
    Citanje fajlova je lazy
    
    Funkcije za citanje i pisanje fajlova rade isto kao i funkcije koje rade 
    sa standardnim ulazom / izlazom samo imaju i file handle argument
        - putStrLn :: String -> IO () -> hPutStrLn :: Handle -> String -> IO ()
    
    Buffering
        - tekstualni fajlovi - linija
        - binarni fajlovi - blok (zavisi od OS-a)
        - hSetBuffering :: Handle -> BufferMode -> IO ()
        - data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

-}

{-  primer File

    import System.IO

    main = do handle <- openFile "vezbe6.hs" ReadMode
              contents <- hGetContents handle
              putStr . unlines. take 5 . lines $ contents
              hClose handle

    main = do withFile "vezbe6.hs" ReadMode (\handle -> do contents <- hGetContents handle
                                                           putStr . unlines. take 5 . lines $ contents)

    main = do contents <- readFile "vezbe6.hs"
              writeFile "tmp.txt" (unlines . take 5 . lines $ contents)

-}

{-  Domaci

    - Napisati program koji enkriptuje i dekriptuje bilo koji tekstualni fajl upotrebom Vigenere cypher-a
        - cmd line parametri: operacija, sifra (kljuc) i putanja do fajla
        - izlaz: novi fajl (enc/dec)
        - mora da bude otporan na greske u funkcionalnom delu
    
    - Napisati program za rad sa telefonskim imenikom
        - podatke cuvati u fajlu (npr. <ime> <br1> <br2> ...)
        - ime fajla u kom se nalazi imenik se zadaje kao cmd line parametar
            - ostalo moze i ne mora
        - operacije koje mora da podrzi: find, list, add, remove
        - mora da bude otporan na greske u funkcionalnom delu

-}
