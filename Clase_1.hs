doble x = 2*x
suma x y = x+y
triple x = 3*x

normaVectorial x1 x2 = sqrt (x1**2 + x2**2)
functionConstante8 x = 8

func n | n == 0 = 1
       | otherwise = 0
   
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1
       
maximo x y | x >= y = x
           | otherwise = y -- o bien podria cubrir esto con x < y


-- Qué hacen estas funciones?  
         
f1 n | n >= 3 = 5
-- me dice que si n es mayor o igual a 3 se mostrará como resultado 5, de lo contrario devolverá una excepción ya que el dominio de la funcion no cubre el 2


f2 n | n >= 3 = 5
     | n <= 1 = 8
-- esta funcion me dice que en caso de que la primera guarda no se cumpla y si se cumple la segunda (n<=1) devolverá 8, de caso de ser un numero mayor a 1 y menor a 3 (no esta contemplado) devolvera  excepcion


f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8
-- en caso de no cumplir que n sea mayor a 3, y n tener el valor de 2, la consola devolverá una expeción undefined, de no cumplir que tampoco tenga valor 2 (los menores a 2) la función devolvera el valor 8


f4 n | n >= 3 = 5
     | n <= 9 = 7
-- en caso de n ser mayor igual a 3, la funcion devolvera 5 de entrada... segunda guarda pide como condición que si un numero es menor o igual a 9 sea 7, pero todo numero mayor o igual  a 3 ya obtuvo el valor de 5 en la primera guarda, asi que solo aplicará que sea 7 todo numero menor a 3




f5 n | n <= 9 = 7
     | n >= 3 = 5
-- en caso de que el numero sea menor igual a 9, n sera de valor 7, de no ser así, en la segunda condición se le asignará valor 5 solo a los numeros mayores a 9 porque a los numeros menores a este ya se les asignó valor 7
   
   
--pater machi    
f n | n == 0 = 1
    | n /= 0 = 0

-- es lo mismo que hacer

f 0 = 1
f n = 0

--saber cantidad de soluciones de una resolvente

cantidadDeSoluciones b c | d > 0 = 2 --agarro el discriminante y me fijo como es
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b**2 - 4*c --hago una variable d para no repetir tantas veces el discriminante

--tipos de datos
maximoInt :: Int -> Int -> Int --recibe 2 Int y devuelve un Int, no es necesario tipar la funcion pero es una buena practica y para algunas funciones querremos solo recibir enteros
maximoInt x y | x >= y = x
           | otherwise = y
           
maximoRac :: Float -> Float -> Float
maximoRac x y | x>=y = x
              | otherwise = y
             
esMayorA9 :: Int -> Bool
esMayorA9 n | n > 9 = True
            | otherwise = False
 
esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True -- si el resto cuando divido por 2 me da 0 es porque es un numero par
        | otherwise = False
       
esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar n)

-- ejemplos mas raros


funcionRara :: Float -> Float -> Bool -> Bool -- recibe 2 float, 1 booleano y devuelve booleano
funcionRara x y z = (x >= y) || z


valorAbsoluto:: Int -> Int
valorAbsoluto x | x >= 0 = x
                | x < 0 = -x
             
maximoAbsoluto:: Int -> Int -> Int
maximoAbsoluto x y | valorAbsoluto x >= valorAbsoluto y = valorAbsoluto x
                   | otherwise = valorAbsoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >=z = x
              | y >= x && y >= z = y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | y == 0 || x == 0 = True
              | otherwise = False

algunoEs0PM ::Float -> Float -> Bool
algunoEs0PM x 0 = True
algunoEs0PM 0 y = True
algunoEs0PM x y = False

ambosSon0:: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False
 
ambosSon0PM:: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM x y = False

esMultiplo:: Int -> Int -> Bool
esMultiplo x y | mod x y == 0 = True
               | otherwise = False

digitoUnidades x = mod x 10

digitoDecenas x = mod x 100