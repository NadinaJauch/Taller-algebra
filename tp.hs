-- SON a y b COPRIMOS?
--aux
mcd :: Integer -> Integer -> Integer 
mcd a 0 = a 
mcd a b = mcd b (mod a b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | mcd a b == 1 = True
                | otherwise = False 

--ES n 2-PSEUDOPRIMO?
--aux
esPrimo :: Integer -> Bool
esPrimo n = minDivisor n == n

--aux
minDivisor :: Integer -> Integer
minDivisor n = minDivisorDesde  2 n

--aux
minDivisorDesde :: Integer -> Integer -> Integer
minDivisorDesde k n | mod n k == 0 = k
                    | otherwise  = minDivisorDesde (k + 1) n

--gral.
esmPseudoprimo :: Integer -> Integer -> Bool
esmPseudoprimo n m | n <= 1 = False
                   | esPrimo n == True = False
                   | mod ((m ^ (n - 1)) - 1) n == 0 = True
                   | otherwise = False

--para dos.  
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n | esmPseudoprimo n 2 == True = True
                 | otherwise = False
         

--SABER LA CANTIDAD DE 3PSEUDOPRIMOS EN UN RANGO             
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n | n == 1 = 0
                        | esmPseudoprimo n 3 == True = 1 + cantidad3Pseudoprimos(n-1)
                        | otherwise = cantidad3Pseudoprimos(n-1)


--CONOCER CUANDO UN NUMERO CUMPLE SER 2 Y 3 PSEUDOPRIMO POR K-ESIMA VEZ

--aux
esInterseccionDe3pp2pp:: Integer -> Bool
esInterseccionDe3pp2pp n | (esmPseudoprimo n 3 == True) && (es2Pseudoprimo n == True) = True
                         | otherwise = False

--aux
kesimaInterseccionDesde :: Integer -> Integer -> Integer -> Integer
kesimaInterseccionDesde n c p | c == n = p - 1
                       | esInterseccionDe3pp2pp p = kesimaInterseccionDesde n (c + 1) (p + 1)
                       | otherwise = kesimaInterseccionDesde n c (p + 1)
                       
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = kesimaInterseccionDesde n 0 2


--ES CARMICHAEL????
esCarmichael :: Integer -> Bool
esCarmichael n | n < 561 = False  
               | esPrimo n == True = False 
               | anterioresSonPPyCop n (n-1) == True = True
               | otherwise = False

--auxiliar para recorrer todo m anterior y ver si es comprimo n con todos esos m anteriores entre 1 y n-1, y también verifico que sean coprimos estos dos, sino, que vuelva a hacer la recurisivdad con el anterior.
anterioresSonPPyCop :: Integer -> Integer -> Bool
anterioresSonPPyCop n m | m < 1 = False
                        | (sonCoprimos n m == False) = anterioresSonPPyCop n (m-1)
                        | (m == 1) && (esmPseudoprimo n m == True) = True
                        | (esmPseudoprimo n m  == True)  = anterioresSonPPyCop n (m-1)
                        | otherwise = False