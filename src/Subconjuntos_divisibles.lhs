% Subconjuntos_divisibles.lhs
% Subconjuntos divisibles
% José A. Alonso Jiménez
% Sevilla, 28 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Abejas, cantores, \\
     no a la miel, sino a las flores.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Subconjuntos_divisibles where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  subconjuntosDivisibles :: [Int] -> [[Int]]
\end{descripcion}   
tal que (subconjuntosDivisibles xs) es la lista de todos los
subconjuntos de xs en los que todos los elementos tienen un factor
común mayor que 1. Por ejemplo,
\begin{descripcion} 
  subconjuntosDivisibles []         ==  [[]]
  subconjuntosDivisibles [1]        ==  [[]]
  subconjuntosDivisibles [3]        ==  [[3],[]]
  subconjuntosDivisibles [1,3]      ==  [[3],[]]
  subconjuntosDivisibles [3,6]      ==  [[3,6],[3],[6],[]]
  subconjuntosDivisibles [1,3,6]    ==  [[3,6],[3],[6],[]]
  subconjuntosDivisibles [2,3,6]    ==  [[2,6],[2],[3,6],[3],[6],[]]
  subconjuntosDivisibles [2,3,6,8]  ==
    [[2,6,8],[2,6],[2,8],[2],[3,6],[3],[6,8],[6],[8],[]]
  length (subconjuntosDivisibles [1..10])  ==  41
  length (subconjuntosDivisibles [1..20])  ==  1097
  length (subconjuntosDivisibles [1..30])  ==  33833
  length (subconjuntosDivisibles [1..40])  ==  1056986
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (foldl1', subsequences)

-- 1ª solución
-- ===========

subconjuntosDivisibles :: [Int] -> [[Int]]
subconjuntosDivisibles xs = filter esDivisible (subsequences xs)

-- (esDivisible xs) se verifica si todos los elementos de xs tienen un
-- factor común mayor que 1. Por ejemplo,
--    esDivisible [6,10,22]  ==  True
--    esDivisible [6,10,23]  ==  False
esDivisible :: [Int] -> Bool
esDivisible [] = True
esDivisible xs = mcd xs > 1

-- (mcd xs) es el máximo común divisor de xs. Por ejemplo,
--    mcd [6,10,22]  ==  2
--    mcd [6,10,23]  ==  1
mcd :: [Int] -> Int
mcd = foldl1' gcd

-- 2ª solución
-- ===========

subconjuntosDivisibles2 :: [Int] -> [[Int]]
subconjuntosDivisibles2 []     = [[]]
subconjuntosDivisibles2 (x:xs) = [x:ys | ys <- yss, esDivisible (x:ys)] ++ yss
  where yss = subconjuntosDivisibles2 xs

-- 3ª solución
-- ===========

subconjuntosDivisibles3 :: [Int] -> [[Int]]
subconjuntosDivisibles3 []     = [[]]
subconjuntosDivisibles3 (x:xs) = filter esDivisible (map (x:) yss) ++ yss
  where yss = subconjuntosDivisibles3 xs

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (subconjuntosDivisibles [1..21])
--    1164
--    (3.83 secs, 5,750,416,768 bytes)
--    λ> length (subconjuntosDivisibles2 [1..21])
--    1164
--    (0.01 secs, 5,400,232 bytes)
--    λ> length (subconjuntosDivisibles3 [1..21])
--    1164
--    (0.01 secs, 5,264,928 bytes)
--    
--    λ> length (subconjuntosDivisibles2 [1..40])
--    1056986
--    (6.95 secs, 8,845,664,672 bytes)
--    λ> length (subconjuntosDivisibles3 [1..40])
--    1056986
--    (6.74 secs, 8,727,141,792 bytes)
\end{code} 
