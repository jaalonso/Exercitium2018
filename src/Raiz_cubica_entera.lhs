% Raiz_cubica__entera.hs
% Raíz cúbica entera.
% José A. Alonso Jiménez
% Sevilla, 30 de noviembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Tras el vivir y el soñar, \\
  está lo que más importa: \\
  despertar.}} {Antonio Machado}


\section*{Enunciado}

\begin{comment}
\begin{code}
module Raiz_cubica_entera where
\end{code}
\end{comment}

Un número x es un cubo si existe un y tal que $x = y^3$. Por ejemplo, 8
es un cubo porque $8 = 2^3$.

Definir la función
\begin{descripcion}
  raizCubicaEntera :: Integer -> Maybe Integer.
\end{descripcion}
tal que (raizCubicaEntera x n) es justo la raíz cúbica del número
natural x, si x es un cubo y Nothing en caso contrario. Por ejemplo,
\begin{descripcion}
  raizCubicaEntera 8             ==  Just 2
  raizCubicaEntera 9             ==  Nothing
  raizCubicaEntera 27            ==  Just 3
  raizCubicaEntera 64            ==  Just 4
  raizCubicaEntera (2^30)        ==  Just 1024
  raizCubicaEntera (10^9000)     ==  Just (10^3000)
  raizCubicaEntera (5 + 10^9000) ==  Nothing
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Numbers.Primes (primeFactors)
import Data.List           (group)
import Test.QuickCheck

-- 1ª definición
-- =============

raizCubicaEntera :: Integer -> Maybe Integer
raizCubicaEntera x = aux 0
  where aux y | y^3 > x   = Nothing
              | y^3 == x  = Just y
              | otherwise = aux (y+1)

-- 2ª definición
-- =============

raizCubicaEntera2 :: Integer -> Maybe Integer
raizCubicaEntera2 x 
  | y^3 == x  = Just y
  | otherwise = Nothing
  where (y:_) = dropWhile (\z -> z^3 < x) [0..]

-- 3ª definición
-- =============

raizCubicaEntera3 :: Integer -> Maybe Integer 
raizCubicaEntera3 1 = Just 1
raizCubicaEntera3 x = aux (0,x)
    where aux (a,b) | d == x    = Just c
                    | c == a    = Nothing
                    | d < x     = aux (c,b)
                    | otherwise = aux (a,c) 
              where c = (a+b) `div` 2
                    d = c^3

-- 4ª definición
-- =============

raizCubicaEntera4 :: Integer -> Maybe Integer
raizCubicaEntera4 x 
  | y^3 == x  = Just y
  | otherwise = Nothing
  where y = floor ((fromIntegral x)**(1 / 3))

-- Nota. La definición anterior falla para números grandes. Por ejemplo,
--    λ> raizCubicaEntera4 (2^30)
--    Nothing
--    λ> raizCubicaEntera (2^30)
--    Just 1024

-- 5ª definición
-- =============

raizCubicaEntera5 :: Integer -> Maybe Integer
raizCubicaEntera5 x  
  | all (==0) [length as `mod` 3 | as <- ass] =
    Just (product [a^((1 + length as) `div` 3) | (a:as) <- ass])
  | otherwise = Nothing  
  where ass = group (primeFactors x)

-- Equivalencia
-- ============

-- La propiedad es                        
prop_raizCubicaEntera :: Integer -> Property
prop_raizCubicaEntera x =
  x >= 0 ==>
  and [raizCubicaEntera x == f x | f <- [ raizCubicaEntera2
                                        , raizCubicaEntera3]]

-- La comprobación es              
--    λ> quickCheck prop_raizCubicaEntera
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================
  
--    λ> raizCubicaEntera (10^18)
--    Just 1000000
--    (1.80 secs, 1,496,137,192 bytes)
--    λ> raizCubicaEntera2 (10^18)
--    Just 1000000
--    (0.71 secs, 712,134,128 bytes)
--    λ> raizCubicaEntera3 (10^18)
--    Just 1000000
--    (0.01 secs, 196,424 bytes)
--
--    λ> raizCubicaEntera2 (5^27)
--    Just 1953125
--    (1.42 secs, 1,390,760,920 bytes)
--    λ> raizCubicaEntera3 (5^27)
--    Just 1953125
--    (0.00 secs, 195,888 bytes)
--
--    λ> raizCubicaEntera3 (10^9000) == Just (10^3000)
--    True
--    (2.05 secs, 420,941,368 bytes)
--    λ> raizCubicaEntera3 (5 + 10^9000) == Nothing
--    True
--    (2.08 secs, 420,957,576 bytes)
--    λ> raizCubicaEntera5 (5 + 10^9000) == Nothing
--    True
--    (0.03 secs, 141,248 bytes)
\end{code}
