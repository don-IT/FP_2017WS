Aufgabe4

Beispiel1



> data Nat = Null | N Nat

> instance Eq Nat where
>    (==) Null Null   =True
>    (==) (N a) (N b) = (a==b)
>    (==) (N a) Null  = False
>    (==) Null (N a)  = False


> instance Ord Nat where
>    compare Null  Null   = EQ
>    compare Null  (N a)  = LT
>    compare (N a)  Null  = GT 
>    compare (N a)  (N b) = compare a b

> instance Show Nat where

>  show x| x== Null ="0"| otherwise = showBinary (go (N x) 0)


> go :: Nat -> Integer -> Integer
> go (N Null) b = b
> go (N x) b = go x (b+1)

> showBinary :: Integer -> String
> showBinary 0 = ""
> showBinary a =  showBinary (div a 2)++ show (mod a 2)


> instance Num Nat where
>  Null + n = n
>  n + Null = n
>  N n + N m = N . N $ n+m


>  Null - N _ = Null
>  n - Null  = n
>  N m - N n = m-n


>  Null  * _ = Null
>  _ * Null  = Null
>  m * N n = m*n+m 

>  fromInteger x
>          | x<=0 = Null
>          | otherwise = N (fromInteger (x-1))

>  signum Null= Null
>  signum (N n)= N Null

>  abs n =n

> instance Enum Nat where
>  fromEnum Null= 0
>  fromEnum (N x) = 1+ fromEnum x
>  toEnum 0 = Null
>  toEnum x= N $ toEnum (x-1)


Beispiel2

> type Wahrheitswert = Bool
> data Name = N1 | N2 | N3 | N4 | N5 deriving (Eq,Ord,Enum,Show)
> newtype Variable = Var Name deriving (Eq,Ord,Show)

> instance Enum Variable where
>  fromEnum (Var name) = fromEnum name
>  toEnum n = Var (toEnum n :: Name)

> data Ausdruck = K Wahrheitswert
> 			| V Variable
>			| Nicht Ausdruck
>			| Und Ausdruck Ausdruck
>			| Oder Ausdruck Ausdruck
>			deriving (Eq,Show)

> type Belegung = Variable -> Wahrheitswert

> auswerten :: Ausdruck ->Belegung -> Wahrheitswert

> auswerten (K k) b
>	| k== True = True
>	| otherwise = False

> auswerten (V v) b
>		| b(v) == True = True
>		| otherwise= False

> auswerten (Nicht a) b
>		| auswerten a b == False = True
>		| otherwise = False

> auswerten (Oder x y) z
>	| auswerten x z == False && auswerten y z == False = False
>	| otherwise = True

> auswerten (Und x y ) z
>	| auswerten x z == True && auswerten y z == True = True
>	| otherwise = False



































 

