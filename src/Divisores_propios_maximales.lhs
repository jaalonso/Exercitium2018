% Divisores_propios_maximales.lhs
% Divisores propios maximales
% José A. Alonso Jiménez
% Sevilla, 20 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 20 de diciembre de 2018}

\begin{comment}
\begin{code}
module Divisores_propios_maximales where
\end{code}
\end{comment}

Se dice que a es un divisor propio maximal de un número b si a es un
divisor de b distinto de b y no existe ningún número c tal que
a < c < b, a es un divisor de c y c es un divisor de b. Por
ejemplo, 15 es un divisor propio maximal de 30, pero 5 no lo es.

Definir las funciones
\begin{descripcion} 
   divisoresPropiosMaximales  :: Integer -> [Integer]
   nDivisoresPropiosMaximales :: Integer -> Integer
\end{descripcion} 
tales que
\begin{itemize}
\item (divisoresPropiosMaximales x) es la lista de los divisores propios
  maximales de x. Por ejemplo,
\begin{descripcion}   
     divisoresPropiosMaximales 30   ==  [6,10,15]
     divisoresPropiosMaximales 420  ==  [60,84,140,210]
     divisoresPropiosMaximales 7    ==  [1]
     length (divisoresPropiosMaximales (product [1..3*10^4])) == 3245
\end{descripcion} 
\item (nDivisoresPropiosMaximales x) es el número de  divisores propios
  maximales de x. Por ejemplo,
\begin{descripcion}   
     nDivisoresPropiosMaximales 30   ==  3
     nDivisoresPropiosMaximales 420  ==  4
     nDivisoresPropiosMaximales 7    ==  1
     nDivisoresPropiosMaximales (product [1..3*10^4])  ==  3245
\end{descripcion} 
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.Numbers.Primes (primeFactors)
import Data.List (genericLength, group, nub)
import Test.QuickCheck

-- 1ª definición de divisoresPropiosMaximales
-- ==========================================

divisoresPropiosMaximales :: Integer -> [Integer]
divisoresPropiosMaximales x =
  [y | y <- divisoresPropios x
     , null [z | z <- divisoresPropios x
               , y < z 
               , z `mod` y == 0]]

-- (divisoresPropios x) es la lista de los divisores propios de x; es
-- decir, de los divisores de x distintos de x. Por ejemplo,
--    divisoresPropios 30  ==  [1,2,3,5,6,10,15]
divisoresPropios :: Integer -> [Integer]
divisoresPropios x =
  [y | y <- [1..x-1]
     , x `mod` y == 0]

-- 2ª definición de divisoresPropiosMaximales
-- ==========================================

divisoresPropiosMaximales2 :: Integer -> [Integer]
divisoresPropiosMaximales2 x =
  reverse [x `div` y | y <- nub (primeFactors x)]

-- Equivalencia de las definiciones de divisoresPropiosMaximales
-- =============================================================

-- La propiedad es
prop_divisoresPropiosMaximales_equiv :: Positive Integer -> Bool
prop_divisoresPropiosMaximales_equiv (Positive x) =
  divisoresPropiosMaximales x == divisoresPropiosMaximales2 x

-- La comprobación es
--    λ> quickCheck prop_divisoresPropiosMaximales_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de divisoresPropiosMaximales
-- ======================================================

--    λ> length (divisoresPropiosMaximales (product [1..10]))
--    4
--    (13.33 secs, 7,037,241,776 bytes)
--    λ> length (divisoresPropiosMaximales2 (product [1..10]))
--    4
--    (0.00 secs, 135,848 bytes)

-- 1ª definición de nDivisoresPropiosMaximales
-- ===========================================

nDivisoresPropiosMaximales :: Integer -> Integer
nDivisoresPropiosMaximales =
  genericLength . divisoresPropiosMaximales
  
-- 2ª definición de nDivisoresPropiosMaximales
-- ===========================================

nDivisoresPropiosMaximales2 :: Integer -> Integer
nDivisoresPropiosMaximales2 =
  genericLength . divisoresPropiosMaximales2
  
-- 3ª definición de nDivisoresPropiosMaximales
-- ===========================================

nDivisoresPropiosMaximales3 :: Integer -> Integer
nDivisoresPropiosMaximales3 =
  genericLength . group . primeFactors

-- Equivalencia de las definiciones de nDivisoresPropiosMaximales
-- ==============================================================

-- La propiedad es
prop_nDivisoresPropiosMaximales_equiv :: Positive Integer -> Bool
prop_nDivisoresPropiosMaximales_equiv (Positive x) =
  nDivisoresPropiosMaximales  x == nDivisoresPropiosMaximales3 x &&
  nDivisoresPropiosMaximales2 x == nDivisoresPropiosMaximales3 x 

-- La comprobación es
--    λ> quickCheck prop_nDivisoresPropiosMaximales_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de nDivisoresPropiosMaximales
-- =======================================================

--    λ> nDivisoresPropiosMaximales2 (product [1..10])
--    4
--    (13.33 secs, 7,037,242,536 bytes)
--    λ> nDivisoresPropiosMaximales2 (product [1..10])
--    4
--    (0.00 secs, 135,640 bytes)
--    λ> nDivisoresPropiosMaximales3 (product [1..10])
--    4
--    (0.00 secs, 135,232 bytes)
--    
--    λ> nDivisoresPropiosMaximales2 (product [1..3*10^4])
--    3245
--    (3.12 secs, 4,636,274,040 bytes)
--    λ> nDivisoresPropiosMaximales3 (product [1..3*10^4])
--    3245
--    (3.06 secs, 4,649,295,056 bytes)
\end{code} 
