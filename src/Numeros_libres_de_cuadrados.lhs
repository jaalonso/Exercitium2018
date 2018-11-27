% \chapter*{Números libres de cuadrados}

\section*{Ejercicio propuesto el 15--11--18}

Un número entero positivo es libre de cuadrados si no es divisible
por el cuadrado de ningún entero mayor que 1. Por ejemplo, 70 es
libre de cuadrado porque sólo es divisible por 1, 2, 5, 7 y 70; en
cambio, 40 no es libre de cuadrados porque es divisible por $2^2$.  

Definir la función
\begin{descripcion}
  libreDeCuadrados :: Integer -> Bool
\end{descripcion}
tal que (libreDeCuadrados x) se verifica si x es libre de cuadrados. 
Por ejemplo,
\begin{descripcion}
  libreDeCuadrados 70                    ==  True
  libreDeCuadrados 40                    ==  False
  libreDeCuadrados 510510                ==  True
  libreDeCuadrados (((10^10)^10)^10)     ==  False
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (nub)

-- 1ª definición
-- =============

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados x = x == product (divisoresPrimos x)

-- (divisoresPrimos x) es la lista de los divisores primos de x. Por
-- ejemplo,  
--    divisoresPrimos 40  ==  [2,5]
--    divisoresPrimos 70  ==  [2,5,7]
divisoresPrimos :: Integer -> [Integer]
divisoresPrimos x = [n | n <- divisores x, primo n]

-- (divisores n) es la lista de los divisores del número n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]  
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
primo :: Integer -> Bool
primo n = divisores n == [1, n]

-- 2ª definición
-- =============

libreDeCuadrados2 :: Integer -> Bool
libreDeCuadrados2 n = 
  null [x | x <- [2..n], rem n (x^2) == 0]

-- 3ª definición
-- =============

libreDeCuadrados3 :: Integer -> Bool
libreDeCuadrados3 n = 
  null [x | x <- [2..floor (sqrt (fromIntegral n))]
          , rem n (x^2) == 0]

-- 4ª definición
-- =============

libreDeCuadrados4 :: Integer -> Bool
libreDeCuadrados4 x =
  factorizacion x == nub (factorizacion x)

-- (factorizacion n) es la lista de factores primos de n. Por ejemplo,  
--    factorizacion 180  ==  [2,2,3,3,5]
factorizacion :: Integer -> [Integer]
factorizacion n | n == 1    = []
                | otherwise = x : factorizacion (div n x)
  where x = menorFactor n

-- (menorFactor n) es el menor divisor de n. Por ejemplo,         
--    menorFactor 15  ==  3
menorFactor :: Integer -> Integer
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- Comparación de eficiencia
-- =========================

--    λ> libreDeCuadrados 510510
--    True
--    (0.76 secs, 89,522,360 bytes)
--    λ> libreDeCuadrados2 510510
--    True
--    (1.78 secs, 371,826,320 bytes)
--    λ> libreDeCuadrados3 510510
--    True
--    (0.01 secs, 0 bytes)
--    λ> libreDeCuadrados4 510510
--    True
--    (0.00 secs, 153,216 bytes)
\end{code}
