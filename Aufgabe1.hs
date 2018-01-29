--Uebung1 
--Author Omar Cehajic
--01527620
--Aufgabe1

type Nat0 = Integer
type Nat1= Integer
type GesamtKugelZahl   = Nat1
type GezogeneKugelZahl = Nat1
type Spiel= (GesamtKugelZahl,GezogeneKugelZahl)
type Gluecksspiel= (Spiel,Spiel)

--anzahlWettKombis rechnet mogliche Kombinationen gezogene Kugel aus beide Topfen

anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis ((x1,y1),(x2,y2)) = racun x1 y1 * racun x2 y2 


-- fac rechnet faktorijel

fac :: Nat1 -> Nat0
fac n | n==0 = 1 |  otherwise = n* fac(n-1)


-- racun dividiert zwei faktoriels

racun :: Nat1->Nat1->Nat0
racun x y = div (fac x) (((fac (x-y))* fac y)) 

--Aufgabe2

fib'  :: Nat0->Nat0

fib'  x | x==0 =0 | x==1 =1 | otherwise = check x [1,1,2] 

check :: Nat0->[Nat0]->Nat0
check x y | x<last(y) =x | x==last(y) =toInteger (length(y)) | otherwise = check x (y ++ [last(y) + last(init(y))])

--Aufgabe3

fibs  :: Nat0->[Nat0]

fibs  x | x==0 =[0] | x==1 =[0,1] | otherwise = add x [0,1] 

add :: Nat0->[Nat0]->[Nat0]
add x y | x==1 = y | otherwise = add (x-1) (y ++ [last(y) + last(init(y))])

--Aufgabe4

verflechten :: [Nat0]->[Nat0]->[Nat0]

verflechten [] y = y
verflechten x [] =x
verflechten (x:xs) (y:ys) = [x] ++ [y] ++ verflechten xs ys  
