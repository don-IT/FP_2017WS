
--Aufgabe1

module Aufgabe6 where

type Wahrheitswert = Bool
data VName = N1 | N2 | N3 | N4 | N5 deriving (Eq, Ord, Enum, Show)
newtype Variable = Var VName deriving (Eq, Ord, Show)

instance Enum Variable where
  fromEnum (Var name) = fromEnum name
  toEnum n = Var (toEnum n :: VName)

data Ausdruck = K Wahrheitswert			--Logische Konstante
     	      	| V Variable 			--Logische Variable
		| Nicht Ausdruck 		--Logische Negation
		| Und Ausdruck Ausdruck		--Logische Konjunktion
		| Oder Ausdruck Ausdruck	--Logische Disjunktion
		| Impl Ausdruck Ausdruck	--Logische Implikation
		| Esgibt Variable Ausdruck	--Existentiell quantifizierter Ausdruck
		| Fueralle Variable Ausdruck 	--Allquantifizierter Ausdruck

type Belegung = Variable -> Wahrheitswert

evaluiere :: Ausdruck -> Belegung -> Wahrheitswert
evaluiere (K x) bel         	   = x
evaluiere (V x) bel 	     	   = bel x
evaluiere (Nicht x) bel      	   = not(evaluiere x bel)
evaluiere (Und x y) bel      	   = (evaluiere x bel) && (evaluiere y bel)
evaluiere (Oder x y) bel     	   = (evaluiere x bel) || (evaluiere y bel)
evaluiere (Impl x y) bel     	   = (evaluiere x bel) `impl` (evaluiere y bel)
evaluiere (Esgibt var ad) bel	   = (evaluiere ad bel) || (evaluiere ad (andereBelegung bel var))
evaluiere (Fueralle var ad) bel    = (evaluiere ad bel) && (evaluiere ad (andereBelegung bel var))

impl :: Bool -> Bool -> Bool
impl True False = False
impl _ _  	= True

--aendert den Wert in der Belegung

andereBelegung :: Belegung -> Variable -> Belegung
andereBelegung belegung v = \x -> (if x == v then (not (belegung v)) else (belegung x))
  
--task funktion

ist_tautologie :: Ausdruck -> Ausdruck -> Wahrheitswert
ist_tautologie add1 add2 = checkAllValues (Var N1) (\var -> True) add1 add2

--Probiert eine Formel in allen moeglichen Belegungen durch. 
--Die Implementation ist nicht effizient, da 64 statt 32 Vergleiche gemacht werden

checkAllValues :: Variable -> Belegung -> Ausdruck -> Ausdruck -> Wahrheitswert
checkAllValues var@(Var N5) belegung add1 add2 = (evaluiere add1 belegung) == (evaluiere add2 belegung) 
	       	       	       	 &&(evaluiere add1 (andereBelegung belegung var)) == (evaluiere add2 (andereBelegung belegung var))
checkAllValues var belegung add1 add2 = (evaluiere add1 belegung) == (evaluiere add2 belegung) 
	       	       	       	 &&(evaluiere add1 (andereBelegung belegung var)) == (evaluiere add2 (andereBelegung belegung var)) 
			         && (checkAllValues (succ var) belegung add1 add2) 
				 && (checkAllValues (succ var) (andereBelegung belegung var) add1 add2)


--task funktion

schreibe :: Ausdruck -> String
schreibe (K b) = show(b)
schreibe (V (Var n)) = show(n)
schreibe (Nicht a) = "(" ++ "neg" ++ " " ++ schreibe a ++ ")"
schreibe (Und a1 a2) = "(" ++ schreibe a1 ++ " " ++ "und" ++ " " ++ schreibe a2 ++")"
schreibe (Oder a1 a2) = "(" ++ schreibe a1 ++ " " ++ "oder" ++ " " ++ schreibe a2 ++")"
schreibe (Impl a1 a2) = "(" ++ schreibe a1 ++ " " ++ "=>" ++ " " ++ schreibe a2 ++")"
schreibe (Esgibt v a) = "(" ++ "EG" ++ " " ++ schreibe (V v) ++ "." ++ " " ++ schreibe a ++ ")"
schreibe (Fueralle v a) = "(" ++ "FA" ++ " " ++ schreibe (V v) ++ "." ++ " " ++ schreibe a ++ ")"


--Aufgabe2


type Nat1		= Int
type Name 		= String
type Alter 		= Nat1
data Geschlecht 	= M | W | X deriving (Eq, Show)
type Gemeinde 		= String
type Strasse 		= String
type Hausnr 		= Nat1
data Person 		= P Name Alter Geschlecht Wohnsitze deriving (Eq, Show)
data Anschrift 		= A Gemeinde Strasse Hausnr deriving (Eq, Show)
type Wohnsitze 		= [Anschrift]
type Melderegister 	= [Person]
data Registerbaum 	= Leer
     			  | Verzweigung Registerbaum Person Registerbaum deriving (Eq, Show)

migration :: Melderegister -> Registerbaum
migration meld = baum meld Leer

--macht ein Baum aus einem Melderegister

baum :: Melderegister -> Registerbaum -> Registerbaum
baum [] regb = regb
baum (p1@(P n a g ws):md_rest) Leer = baum md_rest (Verzweigung Leer p1 Leer)
baum (p1@(P n1 a1 g1 ws1):md_rest) (Verzweigung lb p2@(P n2 a2 g2 ws2) rb) 
  |  n1 <  n2 = baum md_rest (Verzweigung (addLeft p1) p2 rb) 
  |  n1 >  n2 = baum md_rest (Verzweigung lb p2 (addRight p1))
  |  n1 == n2 && (a1 /= a2 || g1 /= g2) = baum md_rest (Verzweigung lb p2 rb)
  |  n1 == n2 && a1 == a2 && g1 == g2 = baum md_rest (Verzweigung lb (P n1 a1 g1 (ws2 ++ ws1)) rb)
    where addLeft :: Person -> Registerbaum
    	  addLeft p = baum [p] lb
	  addRight :: Person -> Registerbaum
	  addRight p = baum [p] rb

--Sortiert die Anschriften jeder Person und delete  Duplikate

bereinige_Anschriften :: Registerbaum -> Registerbaum
bereinige_Anschriften Leer = Leer
bereinige_Anschriften (Verzweigung lb (P n a g ws) rb) = (Verzweigung (bA lb) (P n a g (qsnd ws)) (bA rb)) 
  where bA = bereinige_Anschriften
  	qsnd = quickSortNoDuplicates

instance Ord Anschrift where 
  compare a1@(A gem1 str1 hnr1) a2@(A gem2 str2 hnr2)
    |  a1 == a2						= EQ 
    |  gem1 < gem2 					= LT
    | (gem1 == gem2) && (str1 < str2) 			= LT
    | (gem1 == gem2) && (str1 == str2) && (hnr1 < hnr2) = LT
    | otherwise      	      	       	  	  	= GT

-- sort

quickSortNoDuplicates :: Ord a => [a] -> [a]
quickSortNoDuplicates []     = []
quickSortNoDuplicates (x:xs) = quickSortNoDuplicates(smaller) ++ [x] ++ quickSortNoDuplicates(bigger)
  where smaller = [y | y <- xs, y < x]
  	bigger  = [y | y <- xs, y > x]

rolle_rueckwaerts :: Registerbaum -> Melderegister
rolle_rueckwaerts b = erstelle_mdreg b []

--from Registerbaum to  Melderegister 

erstelle_mdreg :: Registerbaum -> Melderegister -> Melderegister
erstelle_mdreg Leer md = md
erstelle_mdreg (Verzweigung Leer p Leer) md = (p:md)
erstelle_mdreg (Verzweigung Leer p rb) md   = erstelle_mdreg rb (p:md)
erstelle_mdreg (Verzweigung lb p Leer) md   = p:(erstelle_mdreg lb (md))
erstelle_mdreg (Verzweigung lb p rb) md = (erstelle_mdreg rb (p:(erstelle_mdreg lb md)))


