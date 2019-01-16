% Numeracion_de_ternas.lhs
% Numeración de las ternas de números naturales.
% José A. Alonso Jiménez
% Sevilla, 20 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{¿Cabe una comunión cordial entre hombres, que nos
    permita cantar en coro, animados de un mismo sentir?}}  {Antonio
    Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Numeracion_de_ternas where
\end{code}
\end{comment}

Las ternas de números naturales se pueden ordenar como sigue
\begin{descripcion} 
  (0,0,0), 
  (0,0,1),(0,1,0),(1,0,0),
  (0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0),
  (0,0,3),(0,1,2),(0,2,1),(0,3,0),(1,0,2),(1,1,1),(1,2,0),(2,0,1),...
  ...
\end{descripcion} 
   
Definir la función
\begin{descripcion} 
  posicion :: (Int,Int,Int) -> Int
\end{descripcion} 
tal que (posicion (x,y,z)) es la posición de la terna de números
naturales (x,y,z) en la ordenación anterior. Por ejemplo,
\begin{descripcion} 
  posicion (0,1,0)  ==  2
  posicion (0,0,2)  ==  4
  posicion (0,1,1)  ==  5
\end{descripcion}

Comprobar con QuickCheck que
\begin{itemize}
\item la posición de (x,0,0) es x(x²+6x+11)/6
\item la posición de (0,y,0) es y(y²+3y+ 8)/6
\item la posición de (0,0,z) es z(z²+3z+ 2)/6
\item la posición de (x,x,x) es x(9x²+14x+7)/2
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- 1ª solución
-- ===========

posicion :: (Int,Int,Int) -> Int
posicion (x,y,z) = length (takeWhile (/= (x,y,z)) ternas)

-- ternas es la lista ordenada de las ternas de números naturales. Por ejemplo,
--    ghci> take 9 ternas
--    [(0,0,0),(0,0,1),(0,1,0),(1,0,0),(0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0)]
ternas :: [(Int,Int,Int)]
ternas = [(x,y,n-x-y) | n <- [0..], x <- [0..n], y <- [0..n-x]] 

-- 2ª solución
-- ===========

posicion2 :: (Int,Int,Int) -> Int
posicion2 t = aux 0 ternas
  where aux n (t':ts) | t' == t   = n
                      | otherwise = aux (n+1) ts

-- 3ª solución
-- ===========

posicion3 :: (Int,Int,Int) -> Int
posicion3 t = 
  head [n | (n,t') <- zip [0..] ternas, t' == t]

-- 4ª solución
-- ===========

posicion4 :: (Int,Int,Int) -> Int
posicion4 t = fromJust (elemIndex t ternas)

-- 5ª solución
-- ===========

posicion5 :: (Int,Int,Int) -> Int
posicion5 = fromJust . (`elemIndex` ternas)

-- Equivalencia
-- ============

-- La propiedad es
prop_posicion_equiv :: NonNegative Int
                       -> NonNegative Int
                       -> NonNegative Int
                       -> Bool
prop_posicion_equiv (NonNegative x) (NonNegative y) (NonNegative z) =
  all (== posicion (x,y,z)) [f (x,y,z) | f <- [ posicion2
                                              , posicion3
                                              , posicion4
                                              , posicion5
                                              ]]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--   λ> posicion (200,0,0)
--   1373700
--   (2.48 secs, 368,657,528 bytes)
--   λ> posicion2 (200,0,0)
--   1373700
--   (3.96 secs, 397,973,912 bytes)
--   λ> posicion3 (200,0,0)
--   1373700
--   (1.47 secs, 285,831,056 bytes)
--   λ> posicion4 (200,0,0)
--   1373700
--   (0.13 secs, 102,320 bytes)
--   λ> posicion5 (200,0,0)
--   1373700
--   (0.14 secs, 106,376 bytes)

-- Propiedades
-- ===========

-- La 1ª propiedad es
prop_posicion1 :: NonNegative Int -> Bool
prop_posicion1 (NonNegative x) =
  posicion (x,0,0) == x * (x^2 + 6*x + 11) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion1
--    +++ OK, passed 100 tests.

-- La 2ª propiedad es
prop_posicion2 :: NonNegative Int -> Bool
prop_posicion2 (NonNegative y) =
  posicion (0,y,0) == y * (y^2 + 3*y + 8) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion2
--    +++ OK, passed 100 tests.

-- La 3ª propiedad es
prop_posicion3 :: NonNegative Int -> Bool
prop_posicion3 (NonNegative z) =
  posicion (0,0,z) == z * (z^2 + 3*z + 2) `div` 6

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion3
--    +++ OK, passed 100 tests.

-- La 4ª propiedad es
prop_posicion4 :: NonNegative Int -> Bool
prop_posicion4 (NonNegative x) =
  posicion (x,x,x) == x * (9 * x^2 + 14 * x + 7) `div` 2

-- Su comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_posicion4
--    +++ OK, passed 100 tests.
\end{code} 
