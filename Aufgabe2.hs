--Aufgabe2

--Beispiel1



type N1 = Int
p2p :: (N1,N1) -> (N1,N1)
p2p(m,n) = ( div n (help_func (m,n)), div m (help_func (m,n)) )


-- wir rechnen ggT von m und n
help_func :: (N1,N1) -> N1
help_func(m,n) 
		| m < n = help_func(n,m)
		| n==0 = m
		| otherwise = help_func(n, mod m n)


--Beispiel2

type Nat0 = Integer
type Nat1= Integer
type GesamtKugelZahl   = Nat1
type GezogeneKugelZahl = Nat1
type Spiel= (GesamtKugelZahl,GezogeneKugelZahl)
type Gluecksspiel= (Spiel,Spiel)
type AngeboteneSpiele= [Gluecksspiel]

attraktiveSpieleVorne :: AngeboteneSpiele -> [Gluecksspiel]
attraktiveSpieleVorne [] = []
attraktiveSpieleVorne (x:xs) | (rauswerfen x)== True = attraktiveSpieleVorne xs | otherwise = attraktiveSpieleVorne [a | a<- xs, (anzahlWettKombis a) <= (anzahlWettKombis x)] ++ [x] ++ attraktiveSpieleVorne [a | a<-xs,(anzahlWettKombis a) > (anzahlWettKombis x)]

rauswerfen :: Gluecksspiel -> Bool
rauswerfen ((x,y),(m,n)) | x<y || m<n =True | otherwise =False


--anzahlWettKombis rechnet mogliche Kombinationen gezogene Kugel aus beide Topfen

anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis ((x1,y1),(x2,y2)) | rauswerfen ((x1,y1),(x2,y2)) == True = 0| otherwise = racun x1 y1 * racun x2 y2 


-- fac rechnet faktorijel

fac :: Nat1 -> Nat0
fac n | n==0 = 1 |  otherwise = n* fac(n-1)


-- racun dividiert zwei faktoriels

racun :: Nat1->Nat1->Nat0
racun x y = div (fac x) (((fac (x-y))* fac y)) 


--Beispiel3

type Toepfchen =[Int]
type Kroepfchen = [Int]
type Zahlenliste =[Int]

aufteilen :: Zahlenliste -> (Toepfchen , Kroepfchen)
aufteilen [] = ([],[])
aufteilen (x) = help x ([],[])   

help :: Zahlenliste-> (Toepfchen,Kroepfchen)-> (Toepfchen, Kroepfchen)
help [] (gut,notgut)  = (gut, notgut) 
help (x:xs) (gut,notgut) | mod (check1 x) 3 ==0 = help xs (gut ++ [x] , notgut) | otherwise =help xs (gut , notgut ++ [x])  

check1 :: Int -> Int
check1 x | x == 0 =0 | (mod x 3)==1 = 1+ check1 (div x 3) | otherwise = check1 (div x 3)

--Beispiel4

type Nat = [Int]
ziffern = [0,1,2,3,4,5,6,7,8,9]

istGueltig :: Nat->Bool
istGueltig x = check2 x True 

check2 :: Nat ->Bool -> Bool
check2 [] y = y
check2 (x:xs) y | x<0 || x>9 = check2 xs False | otherwise = check2 xs y 

normalForm :: Nat->Nat
normalForm []=[]
normalForm x | (istGueltig x) ==False = [] | otherwise = check3 x True

check3 :: Nat->Bool->Nat
check3 [] True = [0]
check3 x False = x
check3 (x:xs) y | x>0=check3 (x:xs) False | otherwise = check3 xs y

addiere :: Nat->Nat->Nat
addiere x y | (istGueltig x)==False || (istGueltig y)==False=[] | otherwise=check4 x y [] 0

check4 :: Nat->Nat->Nat->Int->Nat
check4 [] [] x r = normalForm ([r]++x)
check4 [] a x r = normalForm ( fuc a [] r ) ++ x
check4 a [] x r = normalForm ( fuc a [] r ) ++ x
check4 a b x r = check4 (init a) (init b) ([(mod ((last b)+(last a)+r) 10)]++x) (div ((last b)+(last a)+r) 10)

fuc :: Nat->Nat-> Int -> Nat
fuc [] y x= [x]++y 
fuc x y z = fuc (init x) ([mod ((last x)+z) 10] ++y) (div ((last x)+z) 10)


subtrahiere :: Nat-> Nat-> Nat
subtrahiere x y | (istGueltig x)==False || (istGueltig y)==False=[] | otherwise=check5 x y [] 0

check5 :: Nat-> Nat->Nat->Int->Nat
check5 [] [] x r | r>0 = [0] | otherwise = normalForm x
check5 [] y x r = []
check5 y [] x r = check5 (init y) [] ([izracunaj (last y) 0 r]++x) (izracunajrest (last y) 0 r)
check5 z y x r =  check5 (init z) (init y) ([izracunaj (last z) (last y) r] ++ x) (izracunajrest (last z) (last y) r)

izracunaj :: Int->Int->Int->Int
izracunaj x y rest | (subtract (subtract x y) rest)<0 =(subtract (subtract (10+x) y) rest)| otherwise = (subtract (subtract x y) rest)

izracunajrest :: Int->Int->Int->Int
izracunajrest x y rest | (subtract (subtract x y) rest)<0 =1 | otherwise =0











