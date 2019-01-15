% Minimo_producto_escalar.lhs
% Mínimo producto escalar.
% José A. Alonso Jiménez
% Sevilla, 6 de enero de 2016
% ---------------------------------------------------------------------

\epigraph {\textit{El escepticismo es una posición vital, no lógica, que
    ni afirma ni niega, se limita a preguntar, y no se asusta de las
    contradicciones.}}  {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Minimo_producto_escalar where
\end{code}
\end{comment}

El producto escalar de los vectores \((a_1,a_2,\dots,a_n)\) y
\((b_1,b_2,\dots,b_n)\) es
\[a_1 b_1 + a_2 b_2 + \dots + a_n * b_n\]

Definir la función
\begin{descripcion} 
  menorProductoEscalar :: (Ord a, Num a) => [a] -> [a] -> a
\end{descripcion} 
tal que (menorProductoEscalar xs ys) es el mínimo de los productos
escalares de las permutaciones de xs y de las permutaciones de
ys. Por ejemplo,
\begin{descripcion} 
  menorProductoEscalar [3,2,5]  [1,4,6]         == 29
  menorProductoEscalar [3,2,5]  [1,4,-6]        == -19
  menorProductoEscalar [0..9]   [0..9]          == 120
  menorProductoEscalar [0..99]  [0..99]         == 161700
  menorProductoEscalar [0..999] [0..999]        == 166167000
  menorProductoEscalar [0..9999] [0..9999]      == 166616670000
  menorProductoEscalar [0..99999] [0..99999]    == 166661666700000
  menorProductoEscalar [0..999999] [0..999999]  == 166666166667000000
\end{descripcion} 
   
\section*{Soluciones}

\begin{code} 
import Data.List (sort, permutations)
import Test.QuickCheck  

-- 1ª solución
menorProductoEscalar :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar xs ys = 
  minimum [sum (zipWith (*) pxs pys) | pxs <- permutations xs,
                                       pys <- permutations ys]       

-- 2ª solución
menorProductoEscalar2 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar2 xs ys = 
  minimum [sum (zipWith (*) pxs ys) | pxs <- permutations xs]

-- 3ª solución
menorProductoEscalar3 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar3 xs ys = 
  sum (zipWith (*) (sort xs) (reverse (sort ys)))

-- Equivalencia
-- ============

-- La propiedad es
prop_menorProductoEscalar :: [Integer] -> [Integer] -> Bool
prop_menorProductoEscalar xs ys =
  menorProductoEscalar3 xs' ys' == menorProductoEscalar  xs' ys' &&
  menorProductoEscalar3 xs' ys' == menorProductoEscalar2 xs' ys' 
  where n   = min (length xs) (length ys)
        xs' = take n xs
        ys' = take n ys

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_menorProductoEscalar
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> menorProductoEscalar1 [0..5] [0..5]
--    20
--    (3.24 secs, 977385528 bytes)
--    λ> menorProductoEscalar2 [0..5] [0..5]
--    20
--    (0.01 secs, 4185776 bytes)
--    
--    λ> menorProductoEscalar2 [0..9] [0..9]
--    120
--    (23.86 secs, 9342872784 bytes)
--    λ> menorProductoEscalar3 [0..9] [0..9]
--    120
--    (0.01 secs, 2580824 bytes)
--    
--    λ> menorProductoEscalar3 [0..10^6] [0..10^6]
--    166666666666500000
--    (2.46 secs, 473,338,912 bytes)
\end{code} 
