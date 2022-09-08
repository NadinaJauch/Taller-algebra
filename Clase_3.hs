--Clase 3: Recursividad

--Primer ejemplo: Factorial
factorial :: Int -> Int
factorial n | n==0 = 1
            | n > 0 = n*factorial(n-1)

--Segundo ejemplo: Funcion que devuelve si es par o no (booleano), para eso se fija en el caso base n = 0 si es par, entonces si no es 0, va a restar 2 hasta que sea 0 o no 
esPar :: Int -> Bool
esPar n | n==0 = True
        | otherwise = esPar(n-2)

--Ejercicios: Realizar las siguientes funciones con recursividad: Fibonacci, la parte entera de un numero, ver si un numero es multiplo de 3
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci(n-1)+fibonacci(n-2)
    
parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = 1 + parteEntera(x-1)
               
--Para multiploTres no esta permitido utilizar mod ni div
multiploTres :: Integer -> Bool
multiploTres x | x == 3 = True
               | x < 3 = False
               | otherwise = multiploTres (x-3)
--para multiplo de tres, hago 2 casos bases, que cuando sea igual a 3 sea verdad, si es menor a 3 no es multiplo de 3, si es mayor puede ser que si, asi que le resto 3 para ver si cae en el 3 o no


--Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n numeros impares. Ej: sumaImpares 3 -> 1+3+5 -> 9
sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | otherwise = 2*n-1 + sumaImpares(n-1)
--la respuesta seria la suma de todos los impares anteriores + el impar n


--Escribir una funci on medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por ejemplo
medioFact :: Integer -> Integer
medioFact n | n == 1 = 1
            | n == 0 = 1
            | otherwise = n * medioFact(n-2)

--Escribir una funcion que determine la suma de dıgitos de un numero positivo. Para esta funcion pueden utilizar div y mod.
--recuerdo como tener el ultimo digito de un numero, y como tener el numero sin ese ultimo digito...
ultimoDigito x = mod x 10
numeroSinUltimoDIgito x = div x 10

--entonces si el numero tiene un solo digito, el valor será el del numero, si es de más digitos, agarro la ultima cifra y la sumo al numero truncado sin el ultimo digito
sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | otherwise = mod x 10 + sumaDigitos(div x 10)  


--Implementar una funci on que determine si todos los d ıgitos de un n umero son iguales.
digitosIguales :: Integer -> Bool
digitosIguales x | x < 10 = True
                 | mod x 10 == mod (div x 10) 10 = digitosIguales(div x 10) --acá comparo 2 booleanos, si el ultimo digito es igual al ultimo digito del numero truncado (el anterior) (True por ejemplo)... entonces comparo con con el numero anterior(vuelvo a ver el numero truncado)
                 | otherwise = False
