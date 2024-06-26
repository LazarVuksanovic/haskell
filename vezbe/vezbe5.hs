{-  Jos malo modula

    Sadrze definicije funkcija i tipova itd.
    
    Sintaksa
        - module <Module_name> (imena, koja, eksportujemo, Tip(Lista, Konstruktora), Tip(..)) where
        - module <Module.Name> (imena) where
    
    Imena pocinju velikim slovom
    
    Grupisemo module tako sto ih stavimo u direktorijum sa imenom grupe
        - dir: Module file: Name.hs
        - voditi racuna o velikim slovima
        - ime modula u kodu mora biti isto kao i ime fajla
    
    Moraju se nalaziti u radnom direktorijum
    
    U GHCi-u ih importujemo sa :load
        - ako zelimo da se ponasaju kao moduli u GHCi-u moramo ih instalirati cabal-om

-}

-- primer Module.Zbir

import Module.Zbir

dupli :: (Num a) => a -> a -> a
dupli x y = zbir x y + zbir x y

{-  Tipovi

    Algebarski tipovi
        - sintaksa
            - data <Type_name> <parametri> <tipa> = Value <Type1> | Constructors <Type2> <Type3>
                   deriving (Type, Class)
        - imena pocinju velikim slovom
        - konstruktori vrednosti su funkcije
            - mogu se parcijalno primeniti, mapirati itd.
        - ako navedemo parametre tipa onda nije konkretan tip nego "template"
        - pre imena tipa se moze staviti typeclass constraint ali se ne preporucuje
        - mogu se definisati rekurzivno
        - kljucna rec "deriving"
            - Haskell sam implementira potrebne funkcije
    
    Sinonimi
        - sintaksa
            - type <Type_name> <parametri> <tipa> = Tip
        - tip ostaje isti samo se pise na drugi nacin
        - mogu biti paramtrizovani
        - nisu konsturktori!!!
    
    Record syntax
        - sintaksa
            - data Ime = {ime1 :: Tip1, ime2 :: Tip2} deriving (Klase)
                - voditi racuna o velikim slovima
        - kompajler automatski generise funkcije za pristup parametrima
            - npr. ime1, ime2 itd.
        - show prikazuje podatke sa imenima
        - kod pattern matchinga se mora navesti ime

-}

-- primer tipovi

data Drugari = Pera | Mika

kojiDrugar:: Drugari -> String
kojiDrugar Pera = "Pera"
kojiDrugar Mika = "Mika"

data Pacijent = Zdrav Int | Bolestan Int [String] deriving (Show)

bolnica :: Pacijent -> String
bolnica (Zdrav x) = "Plati: " ++ show x
bolnica (Bolestan x simptomi)
        | x > 37 = "Hladan tus"
        | otherwise = show simptomi

-- map (Bolestan 23) [["boli ruka"], ["boli noga"]]

data Simptom = Simptom String deriving (Show)
type Temp = Float
type Cena = Int
data Pacijent2 = Zdrav2 Cena | Bolestan2 Temp [Simptom] deriving (Show)

-- map (Bolestan2 23) [[Simptom "boli ruka"], [Simptom "boli noga"]]

data Pravougaonik = Pravougaonik {visina :: Int, sirina :: Int} deriving (Show)

povrsina :: Pravougaonik -> Int
povrsina (Pravougaonik {visina = h, sirina = a}) = h * a

data Osoba a = Osoba a | Niko deriving (Show)

data Maybe' a = Nothing' | Just' a

data Either' a b = Left' a | Right' b

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

{-  Klase tipova

    Sintaksa
        - class Eq a where  
              (==) :: a -> a -> Bool  
              (/=) :: a -> a -> Bool  
              x == y = not (x /= y)  
              x /= y = not (x == y)  
        
        - instance <TypeClass> <Type> where
              <implementacije> = <funkcija>
    
    Nije neophodno definisati funkcije
        - moze samo potpis tipa
    
    Ako su uzajamno rekurzivne instanca moze implementirati samo jednu
    
    Moze imati class constraint
        - odnosi se na tip

-}

-- primer klase

data Voce = Jabuka | Kruska | Sljiva -- deriving (Eq, Show)

class (Eq a, Show a) => Test a where
    (<>) :: a -> a -> Bool
    pisi :: a -> String

instance Test Voce where
    (<>) x y = x /= y
    pisi x = show x

instance Eq Voce where
    Jabuka == Jabuka = True
    Kruska == Kruska = True
    Sljiva == Sljiva = True
    _ == _ = False

instance Show Voce where
    show Jabuka = "Jabuka"
    show Kruska = "Kruska"
    show Sljiva = "Sljiva"

instance {- (Eq m) => -} Eq (Maybe' m) where
    Just' x == Just' y = x == y
    Nothing' == Nothing' = True
    _ == _ = False
