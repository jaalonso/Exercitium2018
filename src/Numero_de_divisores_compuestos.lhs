% Numero_de_divisores_compuestos.hs
% Número de divisores compuestos.
% José A. Alonso Jiménez
% Sevilla, 25 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numero_de_divisores_compuestos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   nDivisoresCompuestos :: Integer -> Integer
\end{descripcion} 
tal que (nDivisoresCompuestos x) es el número de divisores de x que
son compuestos (es decir, números mayores que 1 que no son
primos). Por ejemplo,
\begin{descripcion} 
   nDivisoresCompuestos 30  ==  4
   nDivisoresCompuestos (product [1..11])  ==  534
   nDivisoresCompuestos (product [1..25])  ==  340022
   length (show (nDivisoresCompuestos (product [1..3*10^4]))) == 1948
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (genericLength, group, inits, sort)
import Data.Numbers.Primes (isPrime, primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

nDivisoresCompuestos :: Integer -> Integer
nDivisoresCompuestos =
  genericLength . divisoresCompuestos

-- (divisoresCompuestos x) es la lista de los divisores de x que
-- son números compuestos (es decir, números mayores que 1 que no son
-- primos). Por ejemplo,
--    divisoresCompuestos 30  ==  [6,10,15,30]
divisoresCompuestos :: Integer -> [Integer]
divisoresCompuestos x =
  [y | y <- divisores x
     , y > 1
     , not (isPrime y)]

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Integer -> [Integer]
divisores =
  sort
  . map (product . concat)
  . productoCartesiano
  . map inits
  . group
  . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos xss. Por
-- ejemplo, 
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]
  
-- 2ª solución
-- ===========

nDivisoresCompuestos2 :: Integer -> Integer
nDivisoresCompuestos2 x =
  nDivisores x - nDivisoresPrimos x - 1

-- (nDivisores x) es el número de divisores de x. Por ejemplo, 
--    nDivisores 30  ==  8
nDivisores :: Integer -> Integer
nDivisores x =
  product [1 + genericLength xs | xs <- group (primeFactors x)]

-- (nDivisoresPrimos x) es el número de divisores primos de x. Por
-- ejemplo,  
--    nDivisoresPrimos 30  ==  3
nDivisoresPrimos :: Integer -> Integer
nDivisoresPrimos =
  genericLength . group . primeFactors 

-- 3ª solución
-- ===========

nDivisoresCompuestos3 :: Integer -> Integer
nDivisoresCompuestos3 x =
  nDivisores' - nDivisoresPrimos' - 1
  where xss               = group (primeFactors x)
        nDivisores'       = product [1 + genericLength xs | xs <-xss]
        nDivisoresPrimos' = genericLength xss

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_nDivisoresCompuestos :: (Positive Integer) -> Bool
prop_nDivisoresCompuestos (Positive x) =
  all (== nDivisoresCompuestos x) [f x | f <- [ nDivisoresCompuestos2
                                              , nDivisoresCompuestos3 ]]

-- La comprobación es
--    λ> quickCheck prop_nDivisoresCompuestos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> nDivisoresCompuestos (product [1..25])
--    340022
--    (2.53 secs, 3,145,029,032 bytes)
--    λ> nDivisoresCompuestos2 (product [1..25])
--    340022
--    (0.00 secs, 220,192 bytes)
--    
--    λ> length (show (nDivisoresCompuestos2 (product [1..3*10^4])))
--    1948
--    (5.22 secs, 8,431,630,288 bytes)
--    λ> length (show (nDivisoresCompuestos3 (product [1..3*10^4])))
--    1948
--    (3.06 secs, 4,662,277,664 bytes)
\end{code} 
