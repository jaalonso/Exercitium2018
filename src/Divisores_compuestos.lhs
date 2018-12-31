% Divisores_compuestos.lhs
% Divisores compuestos.
% José A. Alonso Jiménez
% Sevilla, 24 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Divisores_compuestos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   divisoresCompuestos :: Integer -> [Integer]
\end{descripcion} 
tal que (divisoresCompuestos x) es la lista de los divisores de x que
son números compuestos (es decir, números mayores que 1 que no son
primos). Por ejemplo,
\begin{descripcion} 
   divisoresCompuestos 30  ==  [6,10,15,30]
   length (divisoresCompuestos (product [1..11]))  ==  534
   length (divisoresCompuestos (product [1..14]))  ==  2585
   length (divisoresCompuestos (product [1..16]))  ==  5369
   length (divisoresCompuestos (product [1..25]))  ==  340022
\end{descripcion}
 
\section*{Soluciones}

\begin{code} 
import Data.List (group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (isPrime, primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisoresCompuestos :: Integer -> [Integer]
divisoresCompuestos x =
  [y | y <- divisores x
     , y > 1
     , not (isPrime y)]

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Integer -> [Integer]
divisores x =
  [y | y <- [1..x]
     , x `mod` y == 0]

-- 2ª solución
-- ===========

divisoresCompuestos2 :: Integer -> [Integer]
divisoresCompuestos2 x =
  [y | y <- divisores2 x
     , y > 1
     , not (isPrime y)]

-- (divisores2 x) es la lista de los divisores de x. Por ejemplo,
--    divisores2 30  ==  [1,2,3,5,6,10,15,30]
divisores2 :: Integer -> [Integer]
divisores2 x =
  [y | y <- [1..x `div` 2], x `mod` y == 0] ++ [x] 

-- 2ª solución
-- ===========

divisoresCompuestos3 :: Integer -> [Integer]
divisoresCompuestos3 x =
  [y | y <- divisores2 x
     , y > 1
     , not (isPrime y)]

-- (divisores3 x) es la lista de los divisores de x. Por ejemplo,
--    divisores2 30  ==  [1,2,3,5,6,10,15,30]
divisores3 :: Integer -> [Integer]
divisores3 x =
  nub (sort (ys ++ [x `div` y | y <- ys]))
  where ys = [y | y <- [1..floor (sqrt (fromIntegral x))]
                , x `mod` y == 0]
             
-- 4ª solución
-- ===========

divisoresCompuestos4 :: Integer -> [Integer]
divisoresCompuestos4 x =
  [y | y <- divisores4 x
     , y > 1
     , not (isPrime y)]

-- (divisores4 x) es la lista de los divisores de x. Por ejemplo,
--    divisores4 30  ==  [1,2,3,5,6,10,15,30]
divisores4 :: Integer -> [Integer]
divisores4 =
  nub . sort . map product . subsequences . primeFactors

-- 5ª solución
-- ===========

divisoresCompuestos5 :: Integer -> [Integer]
divisoresCompuestos5 x =
  [y | y <- divisores5 x
     , y > 1
     , not (isPrime y)]

-- (divisores5 x) es la lista de los divisores de x. Por ejemplo,
--    divisores5 30  ==  [1,2,3,5,6,10,15,30]
divisores5 :: Integer -> [Integer]
divisores5 =
  sort
  . map (product . concat)
  . productoCartesiano
  . map inits
  . group
  . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo, 
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]
  
-- 6ª solución
-- ===========

divisoresCompuestos6 :: Integer -> [Integer]
divisoresCompuestos6 =
  sort
  . map product
  . compuestos
  . map concat
  . productoCartesiano
  . map inits
  . group
  . primeFactors
  where compuestos xss = [xs | xs <- xss, length xs > 1]  

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_divisoresCompuestos :: (Positive Integer) -> Bool
prop_divisoresCompuestos (Positive x) =
  all (== divisoresCompuestos x) [f x | f <- [ divisoresCompuestos2
                                             , divisoresCompuestos3
                                             , divisoresCompuestos4
                                             , divisoresCompuestos5
                                             , divisoresCompuestos6 ]]

-- La comprobación es
--    λ> quickCheck prop_divisoresCompuestos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> length (divisoresCompuestos (product [1..11]))
--    534
--    (14.59 secs, 7,985,108,976 bytes)
--    λ> length (divisoresCompuestos2 (product [1..11]))
--    534
--    (7.36 secs, 3,993,461,168 bytes)
--    λ> length (divisoresCompuestos3 (product [1..11]))
--    534
--    (7.35 secs, 3,993,461,336 bytes)
--    λ> length (divisoresCompuestos4 (product [1..11]))
--    534
--    (0.07 secs, 110,126,392 bytes)
--    λ> length (divisoresCompuestos5 (product [1..11]))
--    534
--    (0.01 secs, 3,332,224 bytes)
--    λ> length (divisoresCompuestos6 (product [1..11]))
--    534
--    (0.01 secs, 1,869,776 bytes)
--    
--    λ> length (divisoresCompuestos4 (product [1..14]))
--    2585
--    (9.11 secs, 9,461,570,720 bytes)
--    λ> length (divisoresCompuestos5 (product [1..14]))
--    2585
--    (0.04 secs, 17,139,872 bytes)
--    λ> length (divisoresCompuestos6 (product [1..14]))
--    2585
--    (0.02 secs, 10,140,744 bytes)
--    
--    λ> length (divisoresCompuestos2 (product [1..16]))
--    5369
--    (1.97 secs, 932,433,176 bytes)
--    λ> length (divisoresCompuestos5 (product [1..16]))
--    5369
--    (0.03 secs, 37,452,088 bytes)
--    λ> length (divisoresCompuestos6 (product [1..16]))
--    5369
--    (0.03 secs, 23,017,480 bytes)
--    
--    λ> length (divisoresCompuestos5 (product [1..25]))
--    340022
--    (2.43 secs, 3,055,140,056 bytes)
--    λ> length (divisoresCompuestos6 (product [1..25]))
--    340022
--    (1.94 secs, 2,145,440,904 bytes)
\end{code} 
