% Suma_de_segmentos_iniciales.lhs
% Suma de segmentos iniciales.
% José A. Alonso Jiménez
% Sevilla, 7 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Al andar se hace camino, \\
     y al volver la vista atrás \\
     se ve la senda que nunca \\
     se ha de volver a pisar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Suma_de_segmentos_iniciales where
\end{code}
\end{comment}

Los segmentos iniciales de [3,1,2,5] son [3], [3,1], [3,1,2] y
[3,1,2,5]. Sus sumas son 3, 4, 6 y 9, respectivamente. La suma de
dichas sumas es 24.
   
Definir la función
\begin{descripcion} 
  sumaSegmentosIniciales :: [Integer] -> Integer
\end{descripcion} 
tal que (sumaSegmentosIniciales xs) es la suma de las sumas de los
segmentos iniciales de xs. Por ejemplo,
\begin{descripcion} 
  sumaSegmentosIniciales [3,1,2,5]     ==  24
  sumaSegmentosIniciales3 [1..3*10^6]  ==  4500004500001000000
\end{descripcion}

Comprobar con QuickCheck que la suma de las sumas de los segmentos
iniciales de la lista formada por n veces el número uno es el
n-ésimo número triangular; es decir que
\begin{descripcion} 
  sumaSegmentosIniciales (genericReplicate n 1)
\end{descripcion} 
es igual a
\begin{descripcion} 
  n * (n + 1) `div` 2
\end{descripcion}
 
\section*{Soluciones}

\begin{code} 
import Data.List (genericLength, genericReplicate)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaSegmentosIniciales :: [Integer] -> Integer
sumaSegmentosIniciales xs =
  sum [sum (take k xs) | k <- [1.. length xs]]

-- 2ª solución
-- ===========

sumaSegmentosIniciales2 :: [Integer] -> Integer
sumaSegmentosIniciales2 xs =
  sum (zipWith (*) [n,n-1..1] xs)
  where n = genericLength xs

-- 3ª solución
-- ===========

sumaSegmentosIniciales3 :: [Integer] -> Integer
sumaSegmentosIniciales3 xs =
  sum (scanl1 (+) xs)

-- Comprobación de la equivalencia
-- ===============================

-- La propiedad es
prop_sumaSegmentosInicialesEquiv :: [Integer] -> Bool
prop_sumaSegmentosInicialesEquiv xs =
  all (== sumaSegmentosIniciales xs) [f xs | f <- [ sumaSegmentosIniciales2
                                                  , sumaSegmentosIniciales3]]

-- La comprobación es
--   λ> quickCheck prop_sumaSegmentosInicialesEquiv
--   +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--   λ> sumaSegmentosIniciales [1..10^4]
--   166716670000
--   (2.42 secs, 7,377,926,824 bytes)
--   λ> sumaSegmentosIniciales2 [1..10^4]
--   166716670000
--   (0.01 secs, 4,855,176 bytes)
--   
--   λ> sumaSegmentosIniciales2 [1..3*10^6]
--   4500004500001000000
--   (2.68 secs, 1,424,404,168 bytes)
--   λ> sumaSegmentosIniciales3 [1..3*10^6]
--   4500004500001000000
--   (1.54 secs, 943,500,384 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_sumaSegmentosIniciales :: Positive Integer -> Bool
prop_sumaSegmentosIniciales (Positive n) =
  sumaSegmentosIniciales3 (genericReplicate n 1) ==
  n * (n + 1) `div` 2

-- La compronación es
--   λ> quickCheck prop_sumaSegmentosIniciales
--   +++ OK, passed 100 tests.
\end{code} 
