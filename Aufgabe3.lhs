Author Omar Cehajic
01527620
Aufgabe3

Beispiel_1 




> type Zahlenliste    = [Integer]
> type Tripelprimzahl = Integer

> schuerfen :: Zahlenliste -> [Tripelprimzahl]
> schuerfen [] =[]
> schuerfen x = goods x [] [2,3,5] 

> goods :: Zahlenliste-> Zahlenliste -> Zahlenliste ->Zahlenliste
> goods [] y z = y
> goods (x:xs) y z | (check1 x z)== True = goods xs (y++[x]) z | otherwise = goods xs y z 

> check1 :: Integer -> [Integer]->Bool
> check1 x (y:ys) | x == (getvalue (y:ys) 1) = True | x<(getvalue (y:ys) 1) = False | otherwise = check1 x (primlist ys ((last ys)+2)) 

> primlist :: [Integer]-> Integer -> [Integer]
> primlist x y | y==7 = x++[y]| mod y 3 ==0 || mod y 5==0 || mod y 7==0  || (checkwithotherprims y 9)==True  = primlist x (y+2) |otherwise = x++[y]

> getvalue :: [Integer] -> Integer ->Integer
> getvalue [] y = y
> getvalue (x:xs) y = getvalue xs (y*x)

> checkwithotherprims :: Integer->Integer->Bool
> checkwithotherprims x y | y>(div x 2) = False | mod x y ==0 =True | otherwise = checkwithotherprims x (y+2) 

Beispiel2

> newtype Kurs = K Float deriving (Eq,Ord,Show)
> instance Num Kurs where
>     (K k1) + (K k2) = K (k1+k2)
>     (K k1) - (K k2 )= K (k1-k2)
>     (K k1) * (K k2) = K (k1*k2)
>     negate (K k) = K (-k)
>     signum (K k)=K (signum k ) 
>     fromInteger a = K (fromInteger a)

> newtype Pegelstand = Pgl Float deriving (Eq,Ord,Show)
> instance Num Pegelstand where
>     (Pgl k1) + (Pgl k2) = Pgl (k1+k2)
>     (Pgl k1) - (Pgl k2 )= Pgl (k1-k2)
>     (Pgl k1) * (Pgl k2) = Pgl (k1*k2)
>     negate (Pgl k) = Pgl (-k)
>     signum (Pgl k)= Pgl (signum k ) 
>     fromInteger  a = Pgl (fromInteger a)


Beispiel3

> curry3 :: ((a,b,c)->d)-> a->b ->c ->d
> curry3 f a b c = f (a,b,c)

> uncurry3 :: (a-> b -> c -> d)->( a, b, c) ->d
> uncurry3 g (a,b,c) = g a b c

> curry_flip :: ((a,b)->c)-> (b -> a -> c)
> curry_flip f a b = f (b,a)

> uncurry_flip :: (a -> b -> c ) -> ((b,a) ->c)
> uncurry_flip  g (b,a) = g a b




Beispiel 4

> verflechten3 :: [Int] ->[Int] -> [Int] -> [Int]
> verflechten3 x [] [] =x
> verflechten3 [] y [] =y
> verflechten3 [] [] z =z
> verflechten3 x y [] = verflechten x y
> verflechten3 x [] y = verflechten x y
> verflechten3 [] x y = verflechten x y
> verflechten3 (x:xs) (y:ys) (z:zs) = [x] ++ [y] ++ [z] ++ verflechten3 xs ys zs


> verflechten :: [Int]->[Int]->[Int]

> verflechten [] y = y
> verflechten x [] =x
> verflechten (x:xs) (y:ys) = [x] ++ [y] ++ verflechten xs ys
