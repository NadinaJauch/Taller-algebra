import Text.Printf (FieldFormatter)
--Clase 4: Sumatorias

--Primer ejemplo: sumatoria pensada como una sumatoria de 1 a n, donde se suma el indice es igual a n sumado a la sumatoria que va de 1 hasta n-1 incrementando el indice...

sumatoria :: Num t => t -> t
sumatoria n = n + sumatoria(n-1)

--Ejercicios de otras sumatorias:
--Sumatoria desde 0 a n incrementando 2 a la indice
sumatoria1 :: Int -> Int
sumatoria1 0 = 1 --mi caso base
sumatoria1 n= 2^n + sumatoria1 (n-1) --le sumo 2 a la n a la sumatoria que es 2 a la n - 1

sumatoria2:: Int -> Float -> Float
sumatoria2 0 q = 0
sumatoria2 n q = q^n + sumatoria2(n-1) q --q a la n mas la sumatoria hasta n-1

sumatoria3:: Int -> Float -> Float --igual que la anterior pero hasta 2n
sumatoria3 0 q = 0 
sumatoria3 n q =  q^(2*n-1) + q^(2*n) + (sumatoria3 (n-1) q)

--otra forma sería:
sumatoria3':: Int -> Float -> Float
sumatoria3' n q = sumatoria2 (2*n) q --usé la función de sumatoria de q a la n hasta n, pero ahora n es 2*n, entonces va a hacer hasta 2n :)

sumatoria4:: Int -> Float -> Float
sumatoria4 0 q = 1 --caso base
sumatoria4 n q = q^(2*n-1) + q^(2*n) - q^(n-1) + (sumatoria4 (n-1) q) --caso recursivo, sumo los terminos que me interesan a la sumatoria.. q a la 2n-1 + q a la 2n + la sumatoria pero le resto el q a la n-1 porque empieza desde n

--tambien podria ser sumatoria4 n q = (sumatoria3 n q) - (sumatoria2 (n-1) q)

--Ejercicios 2:
fact :: Int -> Int
fact 1 = 1
fact n = n*(fact(n-1))

eAprox :: Int -> Float --aproxima valor de numero e con sumatoria de 0 a n de 1 sobre n!
eAprox 0 = 1 -- C.B..
eAprox n = (eAprox (n-1)) + 1 / (fromIntegral (fact n))

--ahora definir constante e como la aproximacion de e a partir de los 10 primeros terminos de la serie anterior.. es simple, la funcion anterior hasta 10, es decir n= 10
e :: Float
e=eAprox 10

--Ejercicios 3: Sumatorias dobles
--implementar funcion f(n,m): sumatoria de 1 a n * sumatoria de 1 a m con n^m

--voy a hacer dos sumatorias:

sumatoriaDoble:: Int -> Int -> Int
sumatoriaDoble 0 m = 0
sumatoriaDoble n m = (sumatoriaDoble (n-1) m) + round (sumatoria2 m (fromIntegral n))
--usé la sumatoria2 que era la sumatoria de estilo q^n, ahora le mande como q mi parametro m

--Implementar una funcion sumaPotencias q n m que sume todas las potencias de la forma q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m. Es parecido al anterior, en el que usé potencias

sumaPotencias:: Float -> Int -> Int -> Float 
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m*(sumatoria2 n q)

sumaRacionales:: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (fromIntegral (sumatoria n)) / (fromIntegral m) + (sumaRacionales n (m-1))


--Primera funcion tarea:
g1 :: Int -> Int -> Int
g1 0 n = 1
g1 i n = i^n + g1 (i-1) n