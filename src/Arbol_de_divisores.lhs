% Arbol_de_divisores.lhs
% Árbol de divisores
% José A. Alonso Jiménez 
% Sevilla, 21 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 21 de diciembre de 2018}

\begin{comment}
\begin{code}
module Arbol_de_divisores where
\end{code}
\end{comment}

Se dice que a es un divisor propio maximal de un número b si a es un
divisor de b distinto de b y no existe ningún número c tal que
a < c < b, a es un divisor de c y c es un divisor de b. Por
ejemplo, 15 es un divisor propio maximal de 30, pero 5 no lo es.

El árbol de los divisores de un número x es el árbol que tiene como
raíz el número x y cada nodo tiene como hijos sus divisores propios
maximales. Por ejemplo, el árbol de divisores de 30 es
\begin{descripcion} 
          30
          /|\
         / | \
        /  |  \
       /   |   \
      /    |    \
     6    10    15
    / \   / \   / \
   2   3 2   5 3   5
\end{descripcion}
 
Usando el tipo de dato
\begin{descripcion} 
   data Arbol = N Integer [Arbol]
     deriving (Eq, Show)
\end{descripcion} 
el árbol anterior se representa por
\begin{descripcion} 
   N 30
     [N 6
        [N 2 [N 1 []],
         N 3 [N 1 []]],
      N 10
        [N 2 [N 1 []],
         N 5 [N 1 []]],
      N 15
        [N 3 [N 1 []],
         N 5 [N 1 []]]]
\end{descripcion} 
         
Definir las funciones
\begin{descripcion} 
   arbolDivisores             :: Integer -> Arbol
   nOcurrenciasArbolDivisores :: Integer -> Integer -> Integer
\end{descripcion} 
tales que
\begin{itemize}
\item (arbolDivisores x) es el árbol de los divisores del número x. Por
  ejemplo,
\begin{descripcion}   
     λ> arbolDivisores 30
     N 30 [N 6  [N 2 [N 1 []],N 3 [N 1 []]],
           N 10 [N 2 [N 1 []],N 5 [N 1 []]],
           N 15 [N 3 [N 1 []],N 5 [N 1 []]]]
\end{descripcion} 
\item (nOcurrenciasArbolDivisores x y) es el número de veces que aparece
  el número x en el árbol de los divisores del número y. Por ejemplo,
\begin{descripcion}   
     nOcurrenciasArbolDivisores  3 30  ==  2
     nOcurrenciasArbolDivisores  6 30  ==  1
     nOcurrenciasArbolDivisores 30 30  ==  1
     nOcurrenciasArbolDivisores  1 30  ==  6
     nOcurrenciasArbolDivisores  9 30  ==  0
     nOcurrenciasArbolDivisores  2 (product [1..10])  ==  360360
     nOcurrenciasArbolDivisores  3 (product [1..10])  ==  180180
     nOcurrenciasArbolDivisores  5 (product [1..10])  ==  90090
     nOcurrenciasArbolDivisores  7 (product [1..10])  ==  45045
     nOcurrenciasArbolDivisores  6 (product [1..10])  ==  102960
     nOcurrenciasArbolDivisores 10 (product [1..10])  ==  51480
     nOcurrenciasArbolDivisores 14 (product [1..10])  ==  25740
\end{descripcion} 
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.Numbers.Primes (primeFactors)
import Data.List (nub)

data Arbol = N Integer [Arbol]
  deriving (Eq, Show)

-- Definición de arbolDivisores
-- ============================

arbolDivisores :: Integer -> Arbol
arbolDivisores x =
  N x (map arbolDivisores (divisoresPropiosMaximales x))

-- (divisoresPropiosMaximales x) es la lista de los divisores propios
-- maximales de x. Por ejemplo,
--    divisoresPropiosMaximales 30   ==  [6,10,15]
--    divisoresPropiosMaximales 420  ==  [60,84,140,210]
--    divisoresPropiosMaximales 7    ==  [1]
divisoresPropiosMaximales :: Integer -> [Integer]
divisoresPropiosMaximales x =
  reverse [x `div` y | y <- nub (primeFactors x)]

-- Definición de nOcurrenciasArbolDivisores
-- ========================================
  
nOcurrenciasArbolDivisores :: Integer -> Integer -> Integer
nOcurrenciasArbolDivisores x y =
  nOcurrencias x (arbolDivisores y)

-- (nOcurrencias x a) es el número de veces que aprece x en el árbol
-- a. Por ejemplo,
--    nOcurrencias 3 (arbolDivisores 30)  ==  2
nOcurrencias :: Integer -> Arbol -> Integer
nOcurrencias x (N y [])
  | x == y    = 1
  | otherwise = 0
nOcurrencias x (N y zs)
  | x == y    = 1 + sum [nOcurrencias x z | z <- zs]
  | otherwise = sum [nOcurrencias x z | z <- zs]
\end{code} 
