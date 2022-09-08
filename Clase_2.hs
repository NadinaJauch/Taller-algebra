--Que tipo tienen las siguientes funciones y expresiones? 
-- las letras minusculas definen variables de tipo

identidad :: a -> a
identidad x = x

--pueden ser de tipos distintos o no, pero el resultado tiene que coincidir con el elemento que se devuelve
primero :: a -> b -> a
primero x y = x


segundo :: a -> b -> b
segundo x y = y

--pueden ser las 3 variables del mismo tipo o no, pero el resultado tiene que ser int
constante5 :: a -> b -> c -> Int
constante5 x y z = 5

primero2 :: Bool -> Int
primero2 True  = 5

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

--tipos de numeros

triple :: (Num a) => a -> a
triple x = 3* x

--podria comparar cualquier cosa
maximo:: (Ord a) => a -> a -> a
maximo x y | x >= y = x
           | otherwise = y

--distinto tipo mientras sean distintos entre los dos
distintos :: (Eq a) => a -> a -> Bool          
distintos x y = x /= y

--se pide que t pertenezca a floating, ya que con t se realiza sqrt y con u tan solo multiplicacion
pepe :: ( Floating t , Eq t , Num u, Eq u) => t -> t -> u -> Bool
pepe x y z = sqrt ( x + y ) == x && 3* z == 0

--AVERIGUAR EL TIPO ASIGNADO POR HASKELL

-- 5 por ejemplo es Num

f1 :: (Floating t, Ord t) => t -> t -> t -> Bool
f1 x y z = x ** y + z <= x + y ** z

f2 :: (Floating a) => a -> a -> a
f2 x y = ( sqrt x ) / ( sqrt y )

f3 :: (Integral a, Floating a) => a -> a -> a
f3 x y = div ( sqrt x ) ( sqrt y )


f4 :: (Eq t, Floating t) => t -> t -> t -> t
f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
         
       
         
-- devuelve siempre int
f5 :: (Eq t, Floating t) => t -> t -> u -> u
f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
         
         
--TUPLAS
--Ejemplo: Suma de vectores

suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma v w = ((fst v) + (fst w), (snd v) + (snd w))
         
-- con pattern machin
sumaPM :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumaPM (vx,vy) (wx,wy) = (vx + wx, vy + wy)        
             
esOrigen :: ( Float , Float ) -> Bool
esOrigen (0 , 0) = True
esOrigen (_ , _ ) = False    
         
angulo0 :: ( Float , Float ) -> Bool
angulo0 (_ , 0) = True
angulo0 (_ , _ ) = False
