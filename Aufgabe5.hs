--Beispiel1
-- Author Omar Cehajic 1527620

type Nat1           = Int
type Wahrheitswert  = Bool
type Name           = String
type Alter          = Nat1
data Geschlecht     = M | W | X deriving (Show)
type Gemeinde       = String
type Strasse        = String
type Hausnr         = Nat1
data Person         = P Name Alter Geschlecht Wohnsitze deriving (Show)
data Anschrift      = A Gemeinde Strasse Hausnr deriving (Show)
type Wohnsitze      = [Anschrift]
type Von_Anschrift  = Anschrift
type Nach_Anschrift = Anschrift
type Melderegister  = [Person]

instance Eq Geschlecht where
	M == M = True
	W==W = True
	_ == _ = False

instance Eq Person where
	(P n a g w) == (P m b h z) = (a==b) && (n==m) && (g==h) && (w==z)
	_ == _ = False

instance Eq Anschrift where
	(A a b c) == (A d e f) = (a==d) && (b==e) && (c==f)
	_ == _ = False

-- for sorting by name and age

instance Ord Person where 
    compare (P n1 a1 g1 w1) (P n2 a2 g2 w2) = if n1 == n2 then if g1==g2 then compare a1 a2 else compare g1 g2 else compare n1 n2

instance Ord Geschlecht where 
    compare M M = EQ
    compare M W = GT
    compare M X = GT 
    compare W W = EQ 
    compare W M = LT
    compare W X = GT
    compare X X = EQ
    compare X M = LT 
    compare X W = LT
	
-- normal quicksort

sortMelde :: Melderegister -> Melderegister
sortMelde [] = []
sortMelde(p:ps) = sortMelde [m | m<-ps, m <= p] ++ [p] ++ sortMelde [m | m<- ps, m > p]


-- task funkction

einwohner :: Melderegister -> Gemeinde -> [(Name,Geschlecht,Alter)]
einwohner mrg gm = einwohnerHelp (sortMelde mrg) gm

-- change from melderegister to name,gesc,alt

einwohnerHelp :: Melderegister -> Gemeinde -> [(Name,Geschlecht,Alter)] 
einwohnerHelp [] _ = []
einwohnerHelp (m:ms) gem
    | check (getWohnsitze m) gem == True = [((getName m),(getGeschlecht m),(getAlter m))] ++ einwohnerHelp ms gem
    |otherwise = [] ++ einwohnerHelp ms gem



getName :: Person->Name
getName (P n _ _ _)=n

getGeschlecht :: Person->Geschlecht
getGeschlecht (P _ _ g _)=g

getAlter :: Person->Alter
getAlter (P _ a _ _)=a

getWohnsitze :: Person->Wohnsitze
getWohnsitze (P _ _ _ w)=w

getGemeinde :: Anschrift->Gemeinde
getGemeinde (A gem _ _ )=gem

check :: Wohnsitze->Gemeinde->Wahrheitswert
check [] gemeinde=False
check (x:xs) gemeinde
    | ((getGemeinde x)==gemeinde)
      = True
    | otherwise= check xs gemeinde

getStrasse :: Anschrift->Strasse
getStrasse (A _ strasse _)=strasse

getHaus :: Anschrift->Hausnr
getHaus (A _ _ hnr)=hnr

-- task function

durchschnittsalter_mit_Geschlecht_in :: Melderegister->Geschlecht->Gemeinde->Alter
durchschnittsalter_mit_Geschlecht_in x g gemeinde = find x g gemeinde 0 0

-- find all persons with the given gender that live in same place

find :: Melderegister->Geschlecht->Gemeinde->Nat1->Nat1->Alter
find [] g gemeinde a b 
    | b==0
      = 99999
    | otherwise = div a b

find (x:xs) g gemeinde a b=
    if ((show(getGeschlecht x))==(show g))
      then if ((check (getWohnsitze x) gemeinde)==True)
        then find xs g gemeinde (a+(getAlter x)) (b+1)
      else find xs g gemeinde a b
    else find xs g gemeinde a b

-- task function

ist_wohnhaft :: Melderegister->Name->Gemeinde->Wahrheitswert
ist_wohnhaft [] name gemeinde = False
ist_wohnhaft (x:xs) name gemeinde=
    if ((getName x)==name)
      then if ((check (getWohnsitze x) gemeinde)==True)
          then True
      else ist_wohnhaft xs name gemeinde
    else ist_wohnhaft xs name gemeinde


-- task funktion

haben_ausschliesslich_als_Wohnsitz :: Melderegister->Anschrift->[Person]
haben_ausschliesslich_als_Wohnsitz [] ans = []
haben_ausschliesslich_als_Wohnsitz (x:xs) ans=
    if ((help (getWohnsitze x) ans 0)==True)
      then [(set_Wohnsitz [] x)]++haben_ausschliesslich_als_Wohnsitz xs ans
    else [] ++ haben_ausschliesslich_als_Wohnsitz xs ans

-- find the same anschrift

help :: Wohnsitze->Anschrift->Int->Wahrheitswert
help [] ans 0=False
help [] ans 1=True
help (x:xs) ans num=
    if (((getGemeinde x)/=(getGemeinde ans))||((getStrasse x)/=(getStrasse ans))||((getHaus x)/=(getHaus ans)))
      then False
    else help xs ans 1

-- make new one

set_Wohnsitz :: Wohnsitze->Person->Person
set_Wohnsitz wohn (P name alt ges _ )=(P name alt ges wohn)

-- task funktion

ummelden :: Melderegister->Von_Anschrift->Nach_Anschrift->Melderegister
ummelden [] von nach=[]
ummelden (x:xs) von nach=[changeWohnsitz x von nach]++ummelden xs von nach

-- select what to change

changeWohnsitz :: Person->Von_Anschrift->Nach_Anschrift->Person
changeWohnsitz (P name alt ges wohn) von nach=(P name alt ges (change wohn von nach) )

-- changing it

change :: Wohnsitze->Von_Anschrift->Nach_Anschrift->Wohnsitze
change [] von nach=[]
change (x:xs) von nach=
    if (((getGemeinde x)==(getGemeinde von))&&((getStrasse x)==(getStrasse von))&&((getHaus x)==(getHaus von)))
      then [nach]++change xs von nach
    else [x] ++ change xs von nach

-- task function

bereinige_Melderegister :: Melderegister->Melderegister
bereinige_Melderegister []=[]
bereinige_Melderegister (x)=remove x [] 

-- set starting point 

remove:: Melderegister->Melderegister->Melderegister
remove [] y = y
remove (x:xs) y | (removeDuplicates x y False) == False = remove xs (y++[x]) 
		| otherwise = remove xs y 

-- actual removing point

removeDuplicates :: Person->Melderegister-> Bool-> Bool 
removeDuplicates p [] x =x
removeDuplicates p (x:xs) y
    | (((getName p)==(getName x))&&((getAlter p)==(getAlter x))&&((show(getGeschlecht p))==(show(getGeschlecht x)))&& (isEqual (getWohnsitze p) (getWohnsitze x)))== True =True
    |otherwise= removeDuplicates p xs y

-- starting check point from selected Wohnsitze

isEqual :: Wohnsitze->Wohnsitze->Bool
isEqual [] []=True
isEqual x y=
    if ((length x)/=(length y))
      then False
    else isEqualHelp x y

-- second point of checking whonsitze

isEqualHelp :: Wohnsitze->Wohnsitze->Bool
isEqualHelp [] y = True
isEqualHelp (x:xs) y | (gotrough x y)== True = isEqualHelp xs y | otherwise = False

-- checking every anshrift in other wohnsitze

gotrough :: Anschrift -> Wohnsitze -> Bool
gotrough x [] = False
gotrough x (y:ys) 
		| (getStrasse x)/=(getStrasse y) || (getHaus x) /= (getHaus y) || (getGemeinde x) /= (getGemeinde y) = gotrough x ys 			| otherwise =True

--Aufgabe5.2

data Knoten = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | K10 deriving (Eq,Show)
type Graph = Knoten -> [Knoten]
newtype G_Graph = GGr Graph
newtype U_Graph = UGr Graph
data Klassifikation=GG|UG|MGG|MUG deriving (Eq, Show)
data Farbe=Tuerkis|Blau deriving (Eq, Show)
type Faerbung=Knoten->Farbe

instance Enum Knoten where
    fromEnum K1 = 0
    fromEnum K2 = 1
    fromEnum K3 = 2
    fromEnum K4 = 3
    fromEnum K5 = 4
    fromEnum K6 = 5
    fromEnum K7 = 6
    fromEnum K8 = 7
    fromEnum K9 = 8
    fromEnum K10 = 9

    toEnum 0 = K1
    toEnum 1 = K2
    toEnum 2 = K3
    toEnum 3 = K4
    toEnum 4 = K5
    toEnum 5 = K6
    toEnum 6 = K7
    toEnum 7 = K8
    toEnum 8 = K9
    toEnum 9 = K10

    enumFrom k = enumFromTo k maxBound
    enumFromThen k1 k2 = enumFromThenTo k1 k2 bound
        where
            bound | fromEnum k1 >= fromEnum k2 = maxBound
                  | otherwise = minBound

instance Bounded Knoten where
    minBound = K1
    maxBound = K10




knoten = [K1, K2, K3, K4, K5, K6, K7, K8, K9, K10] :: [Knoten]

-- removing duplicates with elem

listWithoutDuplicates :: [Knoten] -> [Knoten]
listWithoutDuplicates [] = []
listWithoutDuplicates (k:ks) = if elem k ks then listWithoutDuplicates ks else [k] ++ listWithoutDuplicates ks

-- task function

ist_minimal :: Graph -> Bool
ist_minimal g = null [x | x <- knoten, length (g(x)) /= length (listWithoutDuplicates (g(x)))]

-- task function

klassifiziere :: Graph -> Klassifikation
klassifiziere gr
    |ung && (ist_minimal gr) = MUG
    |ung && not(ist_minimal gr) = UG
    |not ung && ist_minimal gr = MGG
    |otherwise = GG
    where ung = (both gr K1 (gr K1)) && (both gr K2 (gr K2)) && (both gr K3 (gr K3)) && (both gr K4 (gr K4)) && (both gr K5 (gr K5)) && (both gr K6 (gr K6)) && (both gr K7 (gr K7)) && (both gr K8 (gr K8)) && (both gr K9 (gr K9)) && (both gr K10 (gr K10))

-- check kasifikation

both :: Graph -> Knoten -> [Knoten] -> Bool
both _ _ [] = True
both gr knot (k:ks)
    |elem knot (gr k) = both gr knot ks
    |otherwise = False

-- name saying it

cleanduplikates :: [Knoten] -> [Knoten]
cleanduplikates [] = []
cleanduplikates (k:ks) = [k] ++ cleanduplikates [x | x<- ks, x /= k]

--name saying it

cleanGraph :: Graph -> [Knoten] -> Graph
cleanGraph g [] = g
cleanGraph g (k:ks) = cleanGraph (setNachbarn g k K1 (cleanduplikates (g k))) ks

setNachbarn :: Graph -> Knoten -> Knoten -> [Knoten] -> Graph
setNachbarn g k ck kn = gn
    where gn ck | k == ck = kn
                | otherwise = g ck

goTrough :: Graph -> [Knoten] -> Graph
goTrough g [] = g
goTrough g (k:ks) = goTrough ng ks
    where ng = insertit g k (g k)

insertit :: Graph -> Knoten -> [Knoten] -> Graph
insertit g _ [] = g
insertit g kn (k:ks) = insertit ng kn ks
    where ng = setNachbarn g k K1 ((g k) ++ [kn])

erweitere :: Graph -> U_Graph
erweitere g = UGr (cleanGraph (goTrough g [K1 ..]) [K1 ..])

g1 :: Graph
g1 K1 = [K2,K3]
g1 K2 = [K4]
g1 K3 = [K4]
g1 K4 = [K1]
g1 K5 = []
g1 K6 = []
g1 K7 = []
g1 K8 = []
g1 K9 = []
g1 K10 = []



g2 :: U_Graph
g2 = erweitere g1

extract :: U_Graph -> Graph
extract (UGr g) = g

ist_zweifaerbbar :: U_Graph -> Bool
ist_zweifaerbbar g = usable_ist_zweifaerbbar (extract g) [K1 ..] [] []

usable_ist_zweifaerbbar :: Graph -> [Knoten] -> [Knoten] -> [Knoten] -> Bool
usable_ist_zweifaerbbar _ [] _ _ = True
usable_ist_zweifaerbbar g (k:ks) t b
    |(elem k t) && (elem k b) = False
    |not (elem k t) && not (elem k b) = usable_ist_zweifaerbbar g ks (cleanduplikates (t ++ [k])) (cleanduplikates(b ++ (g k)))
    |not (elem k t) && (elem k b) = usable_ist_zweifaerbbar g ks (cleanduplikates (t ++ (g k))) (b)
    |(elem k t) && not (elem k b) = usable_ist_zweifaerbbar g ks (t) (cleanduplikates(b ++ (g k)))

ist_zweifaerbung :: U_Graph -> Faerbung -> Bool
ist_zweifaerbung (UGr g) frb = usable_ist_zweifaerbung g frb [K1 ..]

usable_ist_zweifaerbung :: Graph -> Faerbung -> [Knoten] -> Bool
usable_ist_zweifaerbung _ _ [] = True
usable_ist_zweifaerbung g frb (k:ks)
    |checkColors (frb k) frb (g k) = usable_ist_zweifaerbung g frb ks
    |otherwise = False

checkColors :: Farbe -> Faerbung-> [Knoten] -> Bool
checkColors _ _ [] = True
checkColors farb frbng (k:ks)
    |frbng k == farb = False
    |otherwise = checkColors farb frbng ks
