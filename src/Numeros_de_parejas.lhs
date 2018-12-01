% Numeros_de_parejas.hs
% Número de parejas.
% José A. Alonso Jiménez
% Sevilla, 20 de Noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Numeros_de_parejas where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 20--11--18}

Definir la función
\begin{descripcion}
  nParejas :: Ord a => [a] -> Int
\end{descripcion}
tal que (nParejas xs) es el número de parejas de elementos iguales en
xs. Por ejemplo,
\begin{descripcion}
  nParejas [1,2,2,1,1,3,5,1,2]        ==  3
  nParejas [1,2,1,2,1,3,2]            ==  2
  nParejas [1..2*10^6]                ==  0
  nParejas2 ([1..10^6] ++ [1..10^6])  ==  1000000
\end{descripcion}

En el primer ejemplos las parejas son (1,1), (1,1) y (2,2). En el
segundo ejemplo, las parejas son (1,1) y (2,2).

Comprobar con QuickCheck que para toda lista de enteros xs, el número
de parejas de xs es igual que el número de parejas de la inversa de
xs.

\section*{Soluciones}

\begin{code}
import Test.QuickCheck
import Data.List ((\\), group, sort)

-- 1ª solución
nParejas :: Ord a => [a] -> Int
nParejas []     = 0
nParejas (x:xs) | x `elem` xs = 1 + nParejas (xs \\ [x])
                | otherwise   = nParejas xs

-- 2ª solución
nParejas2 :: Ord a => [a] -> Int
nParejas2 xs =
  sum [length ys `div` 2 | ys <- group (sort xs)]

-- 3ª solución
nParejas3 :: Ord a => [a] -> Int
nParejas3 = sum . map (`div` 2). map length . group . sort

-- 4ª solución
nParejas4 :: Ord a => [a] -> Int
nParejas4 = sum . map ((`div` 2) . length) . group . sort

-- Equivalencia
prop_equiv :: [Int] -> Bool
prop_equiv xs =
  nParejas xs == nParejas2 xs &&
  nParejas xs == nParejas3 xs &&
  nParejas xs == nParejas4 xs 

-- Comprobación:
--    λ> quickCheck prop_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
--    λ> nParejas [1..20000]
--    0
--    (2.54 secs, 4,442,808 bytes)
--    λ> nParejas2 [1..20000]
--    0
--    (0.03 secs, 12,942,232 bytes)
--    λ> nParejas3 [1..20000]
--    0
--    (0.02 secs, 13,099,904 bytes)
--    λ> nParejas4 [1..20000]
--    0
--    (0.01 secs, 11,951,992 bytes)

-- Propiedad:
prop_nParejas :: [Int] -> Bool
prop_nParejas xs =
  nParejas xs == nParejas (reverse xs)

-- Compropación
comprueba :: IO ()
comprueba = quickCheck prop_nParejas

-- Comprobación:
--    λ> comprueba
--    +++ OK, passed 100 tests.
\end{code}
