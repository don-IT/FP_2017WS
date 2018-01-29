-- Uebungsblatt7
--Aufgabe1
--Omar Cehajic
--1527620


type Wahrheitswert = Bool
data Name          = N1 | N2 | N3 | N4 | N5 deriving (Eq,Show)
newtype Variable   = Var Name deriving (Eq,Show)
data Ausdruck = K Wahrheitswert                 -- Logische Konstante
   | V Variable                    -- Logische Variable
   | Nicht Ausdruck                -- Logische Negation
   | Und Ausdruck Ausdruck         -- Logische Konjunktion
   | Oder Ausdruck Ausdruck        -- Logische Disjunktion
   | Impl Ausdruck Ausdruck        -- Logische Implikation
   | Esgibt Variable Ausdruck      -- Existentiell quantifizierter Ausdruck
   | Fueralle Variable Ausdruck    -- Allquantifizierter Ausdruck
   deriving (Eq,Show)


nnf :: Ausdruck->Ausdruck
nnf (K True)=(K True)
nnf (K False)=(K False)
nnf (Nicht (K True))=(K False)
nnf (Nicht (K False))=(K True)
nnf (V var)=(V var)
nnf (Und a b)=(Und (nnf a) (nnf b))
nnf (Oder a b)=(Oder (nnf a) (nnf b))
nnf (Impl a b)=(Impl (nnf a) (nnf b))
nnf (Esgibt (Var a) b)=(Esgibt (Var a) (nnf b))
nnf (Fueralle (Var a) b)=(Fueralle (Var a) (nnf b))
nnf (Nicht (Nicht a))=(nnf a)
nnf (Nicht (V var))=(Nicht (V var))
nnf (Nicht (Und a b))=(Oder (nnf (Nicht a)) (nnf (Nicht b)))
nnf (Nicht (Impl a b))=(Und (nnf a) (nnf (Nicht b)))
nnf (Nicht (Esgibt (Var a) b))=(Fueralle (Var a) (nnf (Nicht b)))
nnf (Nicht (Fueralle (Var a) b))=(Esgibt (Var a) (nnf (Nicht b)))


--Aufgabe2
 
type Nat0 = Int
type Nat1 = Int
type Anzahl_Blutkonserven = Nat0
type Reiseproviant = Anzahl_Blutkonserven
type Reisedauer = Nat1   -- In vollen Std., ausschliesslich Werte von 1..12
type Abfahrtszeit = Nat0 -- In vollen Std., ausschliesslich Werte von 0..23
type Ankunftszeit = Nat0 -- In vollen Std., ausschliesslich Werte von 0..23
data Stadt = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 deriving Show
type Ausgangsort = Stadt
type Zielort = Stadt
type Abfahrtsort = Stadt
type Ankunftsort = Stadt
type Relation = (Abfahrtsort,Abfahrtszeit,Ankunftsort,Ankunftszeit)
type Reiseplan = [Relation]
type Fahrplan = Abfahrtsort -> [(Ankunftsort,Abfahrtszeit,Reisedauer)] -- Total def.
 
instance Eq Stadt where
  (==) S1 S1 = True
  (==) S2 S2 = True
  (==) S3 S3 = True
  (==) S4 S4 = True
  (==) S5 S5 = True
  (==) S6 S6 = True
  (==) S7 S7 = True
  (==) S8 S8 = True
  (==) S9 S9 = True
  (==) S10 S10 = True
  (==) _ _ = False

reise_planer :: Fahrplan -> Ausgangsort -> Zielort -> Maybe Reiseplan
reise_planer fp start finish = getShortest (getPosWays start 0 finish [] (fp start) [] fp) Nothing Nothing
 
proviant_planer :: Fahrplan -> Ausgangsort -> Zielort -> Maybe Reiseproviant
proviant_planer fp start finish = smallestProv (getPosWays start 0 finish [] (fp start) [] fp) Nothing Nothing
 
 

 
getPosWays :: Stadt -> Nat0 -> Zielort -> [Stadt] -> [(Ankunftsort,Abfahrtszeit,Reisedauer)] -> Reiseplan -> Fahrplan-> [Maybe Reiseplan]
getPosWays current time end visited [] rp fp
  |current == end = [Just rp]
  |otherwise = [Nothing]

getPosWays current time end visited connections@((akOrt,abZeit,dauer):cx) rp fp
  |current == end = [Just rp]
  |connections == [] = [Nothing]
  |(abZeit <= 6 || abZeit >= 18) && ((modTime abZeit dauer) <= 6 || (modTime abZeit dauer) >= 18) && (not (hasVisited visited akOrt))= getPosWays current time end visited cx rp fp ++ (getPosWays akOrt (modTime abZeit dauer) end (visited ++ [current]) (fp akOrt) (rp ++ [(current,abZeit,akOrt,(modTime abZeit dauer))]) fp)
  |otherwise = getPosWays current time end visited cx rp fp
 
 
getKonserven :: Reiseplan -> Anzahl_Blutkonserven-> Anzahl_Blutkonserven
getKonserven [] _ = 0
getKonserven (r:[]) anz = anz
getKonserven ((abOrt,abZeit,anOrt,anZeit):(abOrt2,abZeit2,anOrt2,anZeit2):rs) anz
  |(anZeit >= 18 && abZeit2 <=6) || (anZeit >= 18 && abZeit2 >= 18 && abZeit2 >= anZeit) || (anZeit <= 6 && abZeit2 <= 6 && abZeit2 >= anZeit) = getKonserven ((abOrt2,abZeit2,anOrt2,anZeit2):rs) anz
  |otherwise = getKonserven ((abOrt2,abZeit2,anOrt2,anZeit2):rs) (anz + 1)
 
getShortest :: [Maybe Reiseplan] -> Maybe Anzahl_Blutkonserven -> Maybe Reiseplan -> Maybe Reiseplan
getShortest [] anz best = best
getShortest ((Nothing):rs) anz best = getShortest rs anz best
getShortest ((Just r):rs) anz best
  |compareWnothing anz (getKonserven r 0) = getShortest rs (Just (getKonserven r 0)) (Just r)
  |otherwise = getShortest rs anz best
 
smallestProv :: [Maybe Reiseplan] -> Maybe Anzahl_Blutkonserven -> Maybe Reiseplan -> Maybe Anzahl_Blutkonserven
smallestProv [] anz best = anz
smallestProv ((Nothing):rs) anz best = smallestProv rs anz best
smallestProv ((Just r):rs) anz best
  |compareWnothing anz (getKonserven r 0) = smallestProv rs (Just (getKonserven r 0)) (Just r)
  |otherwise = smallestProv rs anz best

hasVisited :: [Stadt] -> Stadt -> Bool
hasVisited [] _ = False
hasVisited (s:xs) t
  |t == s = True
  |otherwise = hasVisited xs t
 
modTime :: Int -> Int -> Ankunftszeit
modTime a b = mod (a + b) 24
 
 
compareWnothing :: Maybe Anzahl_Blutkonserven -> Anzahl_Blutkonserven -> Bool
compareWnothing Nothing i = True
compareWnothing (Just x) y = (y < x)
 

