% Matriz_de_minimas_distancias.lhs
% Matriz de mínimas distancias
% José A. Alonso Jiménez
% Sevilla, 22 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{La primavera ha venido. \\
     Nadie sabe como ha sido.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Matriz_de_minimas_distancias where
\end{code}
\end{comment}

Definir las funciones
\begin{descripcion} 
  minimasDistancias             :: Matrix Int -> Matrix Int
  sumaMinimaDistanciasIdentidad :: Int -> Int
\end{descripcion}   
tales que
\begin{itemize}
\item (mininasDistancias a) es la matriz de las mínimas distancias de
  cada elemento de a hasta alcanzar un 1 donde un paso es un
  movimiento hacia la izquierda, derecha, arriba o abajo. Por
  ejemplo,
\begin{descripcion}   
  λ> minimasDistancias (fromLists [[0,1,1],[0,0,1]])
  ( 1 0 0 )
  ( 2 1 0 )
  λ> minimasDistancias (fromLists [[0,0,1],[1,0,0]])
  ( 1 1 0 )
  ( 0 1 1 )
  λ> minimasDistancias (identity 5)
  ( 0 1 2 3 4 )
  ( 1 0 1 2 3 )
  ( 2 1 0 1 2 )
  ( 3 2 1 0 1 )
  ( 4 3 2 1 0 )
\end{descripcion} 
\item (sumaMinimaDistanciasIdentidad n) es la suma de los elementos de la
  matriz de las mínimas distancias correspondiente a la matriz
  identidad de orden n. Por ejemplo,
\begin{descripcion}   
  sumaMinimaDistanciasIdentidad 5       ==  40
  sumaMinimaDistanciasIdentidad (10^2)  ==  333300
  sumaMinimaDistanciasIdentidad (10^4)  ==  333333330000
  sumaMinimaDistanciasIdentidad (10^6)  ==  333333333333000000
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code}      
import Data.Matrix     (Matrix, (!), matrix, identity, fromLists, nrows, ncols)
import Data.Maybe      (isJust, fromJust)
import Test.QuickCheck 

-- 1ª definición de minimasDistancias
-- ==================================

minimasDistancias :: Matrix Int -> Matrix Int
minimasDistancias a = 
  matrix (nrows a) (ncols a) (\(i,j) -> minimaDistancia (i,j) a) 
 
minimaDistancia :: (Int,Int) -> Matrix Int -> Int
minimaDistancia (a,b) p =
  minimum [distancia (a,b) (c,d) | (c,d) <- unos p]
 
unos :: Matrix Int -> [(Int,Int)]
unos p = [(i,j) | i <- [1..nrows p]
                , j <- [1..ncols p]
                , p ! (i,j) == 1]
 
distancia :: (Int,Int) -> (Int,Int) -> Int
distancia (a,b) (c,d) = abs (c - a) + abs (d - b)

-- 2ª definición de minimasDistancias
-- ==================================

minimasDistancias2 :: Matrix Int -> Matrix Int
minimasDistancias2 a = fmap fromJust (aux (matrizInicial a))
  where aux b | Nothing `elem` c = aux c
              | otherwise        = c
          where c = propagacion b

-- (matrizInicial a) es la matriz que tiene (Just 0) en los elementos de
-- a iguales a 1 y Nothing en los restantes. Por ejemplo,
--    λ> matrizInicial (fromLists [[0,0,1],[1,0,0]])
--    ( Nothing Nothing  Just 0 )
--    (  Just 0 Nothing Nothing )
matrizInicial :: Matrix Int -> Matrix (Maybe Int)
matrizInicial a = matrix m n f
  where m = nrows a
        n = ncols a
        f (i,j) | a ! (i,j) == 1 = Just 0
                | otherwise      = Nothing

-- (propagacion a) es la matriz obtenida cambiando los elementos Nothing
-- de a por el sigiente del mínomo de los valores de sus vecinos. Por
-- ejemplo,
--    λ> propagacion (fromLists [[0,1,1],[0,0,1]])
--    (  Just 1  Just 0  Just 0 )
--    ( Nothing  Just 1  Just 0 )
--    
--    λ> propagacion it
--    ( Just 1 Just 0 Just 0 )
--    ( Just 2 Just 1 Just 0 )
propagacion :: Matrix (Maybe Int) -> Matrix (Maybe Int)
propagacion a = matrix m n f
  where
    m = nrows a
    n = ncols a
    f (i,j) | isJust x  = x
            | otherwise = siguiente (minimo (valoresVecinos a (i,j)))
      where x = a ! (i,j)

-- (valoresVecinos a p) es la lista de los valores de los vecinos la
-- posición p en la matriz a. Por ejemplo,             
--    λ> a = fromList 3 4 [1..]
--    λ> a
--    (  1  2  3  4 )
--    (  5  6  7  8 )
--    (  9 10 11 12 )
--    
--    λ> valoresVecinos a (1,1)
--    [5,2]
--    λ> valoresVecinos a (2,3)
--    [3,11,6,8]
--    λ> valoresVecinos a (2,4)
--    [4,12,7]
valoresVecinos :: Matrix a -> (Int,Int) -> [a]
valoresVecinos a (i,j) = [a ! (k,l) | (k,l) <- vecinos m n (i,j)]
  where m = nrows a
        n = ncols a

-- (vecinos m n p) es la lista de las posiciones vecinas de la posición
-- p en la matriz a; es decir, los que se encuentran a su izquierda,
-- derecha, arriba o abajo. por ejemplo,
--    vecinos 3 4 (1,1)  ==  [(2,1),(1,2)]
--    vecinos 3 4 (2,3)  ==  [(1,3),(3,3),(2,2),(2,4)]
--    vecinos 3 4 (2,4)  ==  [(1,4),(3,4),(2,3)]
vecinos :: Int -> Int -> (Int,Int) -> [(Int,Int)]
vecinos m n (i,j) = [(i - 1,j)     | i > 1] ++
                    [(i + 1,j)     | i < m] ++
                    [(i,    j - 1) | j > 1] ++
                    [(i,    j + 1) | j < n]

-- (minimo xs) es el mínimo de la lista de valores opcionales xs
-- (considerando Nothing como el mayor elemento). Por ejemplo,
--    minimo [Just 3, Nothing, Just 2]  ==  Just 2
minimo :: [Maybe Int] -> Maybe Int
minimo = foldr1 minimo2

-- (minimo2 x y) es el mínimo de los valores opcionales x e y
-- (considerando Nothing como el mayor elemento). Por ejemplo,
--    minimo2 (Just 3) (Just 2)  ==  Just 2
--    minimo2 (Just 1) (Just 2)  ==  Just 1
--    minimo2 (Just 1) Nothing   ==  Just 1
--    minimo2 Nothing (Just 2)   ==  Just 2
--    minimo2 Nothing Nothing    ==  Nothing
minimo2 :: Maybe Int -> Maybe Int -> Maybe Int
minimo2 (Just x) (Just y) = Just (min x y)
minimo2 Nothing  (Just y) = Just y
minimo2 (Just x) Nothing  = Just x
minimo2 Nothing  Nothing  = Nothing

-- (siguiente x) es el siguiente elemento del opcional x (considerando
-- Nothing como el infinito). Por ejemplo, 
--    siguiente (Just 3)  ==  Just 4
--    siguiente Nothing  ==  Nothing
siguiente :: Maybe Int -> Maybe Int
siguiente (Just x) = Just (1 + x)
siguiente Nothing  = Nothing

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximum (minimasDistancias (identity 40))
--    39
--    (3.85 secs, 654,473,496 bytes)
--    λ> maximum (minimasDistancias2 (identity 40))
--    39
--    (0.50 secs, 75,079,912 bytes)

-- 1ª definición de sumaMinimaDistanciasIdentidad
-- ==============================================

sumaMinimaDistanciasIdentidad :: Int -> Int
sumaMinimaDistanciasIdentidad n =
  sum (minimasDistancias (identity n))

-- 2ª definición de sumaMinimaDistanciasIdentidad
-- ==============================================

sumaMinimaDistanciasIdentidad2 :: Int -> Int
sumaMinimaDistanciasIdentidad2 n =
  n*(n^2-1) `div` 3

-- Equivalencia de las definiciones de sumaMinimaDistanciasIdentidad
-- =================================================================

-- La propiedad es
prop_MinimaDistanciasIdentidad :: Positive Int -> Bool
prop_MinimaDistanciasIdentidad (Positive n) =
  sumaMinimaDistanciasIdentidad n == sumaMinimaDistanciasIdentidad2 n

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=50}) prop_MinimaDistanciasIdentidad
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de sumaMinimaDistanciasIdentidad
-- ==========================================================

-- La comparación es
--    λ> sumaMinimaDistanciasIdentidad 50
--    41650
--    (0.24 secs, 149,395,744 bytes)
--    λ> sumaMinimaDistanciasIdentidad 100
--    333300
--    (1.98 secs, 1,294,676,272 bytes)
--    λ> sumaMinimaDistanciasIdentidad 200
--    2666600
--    (17.96 secs, 11,094,515,016 bytes)
--    
--    λ> sumaMinimaDistanciasIdentidad2 50
--    41650
--    (0.00 secs, 126,944 bytes)
--    λ> sumaMinimaDistanciasIdentidad2 100
--    333300
--    (0.00 secs, 126,872 bytes)
--    λ> sumaMinimaDistanciasIdentidad2 200
--    2666600
--    (0.00 secs, 131,240 bytes)
--
-- Resumidamente, el tiempo es
--
--    +-----+---------+--------+
--    |   n | 1ª def. | 2ª def |
--    +-----+---------+--------+
--    |  50 |  0.24   | 0.00   |
--    | 100 |  1.98   | 0.00   |
--    | 200 | 17.96   | 0.00   | 
--    +-----+---------+--------+
\end{code} 
