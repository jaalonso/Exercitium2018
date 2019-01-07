% Arbol_de_subconjuntos.lhs
% Árbol de subconjuntos.
% José A. Alonso Jiménez
% Sevilla, 28 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph
 {\textit{Nunca traces tu frontera, \\
  ni cuides de tu perfil;   \\
  todo eso es cosa de fuera.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Arbol_de_subconjuntos where
\end{code}
\end{comment}

Se dice que A es un subconjunto maximal de B si A ⊂ B y no existe
ningún C tal que A ⊂ C y C ⊂ B. Por ejemplo, {2,5} es un subconjunto
maximal de {2,3,5], pero {3] no lo es.

El árbol de los subconjuntos de un conjunto A es el árbol que tiene
como raíz el conjunto A y cada nodo tiene como hijos sus subconjuntos
maximales. Por ejemplo, el árbol de subconjuntos de [2,3,5] es 
\begin{descripcion} 
         [2, 3, 5]
         /   |  \
        /    |   \  
       /     |    \   
      /      |     \
     /       |      \
   [5,3]   [2,3]   [2,5]  
   /  \    /  \    /  \  
  [3] [5] [3] [2] [5] [2]
   |   |   |   |   |   | 
  [ ] [ ] [ ] [ ] [ ] [ ]
\end{descripcion} 
   
Usando el tipo de dato
\begin{descripcion} 
  data Arbol = N Integer [Arbol]
    deriving (Eq, Show)
\end{descripcion} 
el árbol anterior se representa por
\begin{descripcion} 
  N [2,5,3]
    [N [5,3]
       [N [3]
          [N [] []],
        N [5]
          [N [] []]],
     N [2,3]
       [N [3]
          [N [] []],
        N [2]
          [N [] []]],
     N [2,5]
       [N [5]
          [N [] []],
        N [2]
          [N [] []]]]
\end{descripcion} 

Definir las funciones
\begin{descripcion} 
  arbolSubconjuntos :: [Int] -> Arbol 
  nOcurrenciasArbolSubconjuntos :: [Int] -> [Int] -> Int
\end{descripcion} 
tales que
\begin{itemize}
\item (arbolSubconjuntos x) es el árbol de los subconjuntos de xs. Por
  ejemplo,
\begin{descripcion}   
  λ> arbolSubconjuntos [2,5,3]
  N [2,5,3] [N [5,3] [N [3] [N [] []],N [5] [N [] []]],
             N [2,3] [N [3] [N [] []],N [2] [N [] []]],
             N [2,5] [N [5] [N [] []],N [2] [N [] []]]]
\end{descripcion} 
\item (nOcurrenciasArbolSubconjuntos xs ys) es el número de veces que aparece
  el conjunto xs en el árbol de los subconjuntos de ys. Por ejemplo,
\begin{descripcion}   
  nOcurrenciasArbolSubconjuntos []      [2,5,3]  ==  6
  nOcurrenciasArbolSubconjuntos [3]     [2,5,3]  ==  2
  nOcurrenciasArbolSubconjuntos [3,5]   [2,5,3]  ==  1
  nOcurrenciasArbolSubconjuntos [3,5,2] [2,5,3]  ==  1
\end{descripcion} 
\end{itemize}

Comprobar con QuickChek que, para todo entero positivo n, el número
de ocurrencia de un subconjunto xs de [1..n] en el árbol de los
subconjuntos de [1..n] es el factorial de n-k (donde k es el número
de elementos de xs). 

\section*{Soluciones}

\begin{code} 
import Data.List (delete, nub, sort)
import Test.QuickCheck

data Arbol = N [Int] [Arbol]
  deriving (Eq, Show)

arbolSubconjuntos :: [Int] -> Arbol 
arbolSubconjuntos xs =
  N xs (map arbolSubconjuntos (subconjuntosMaximales xs))

-- (subconjuntosMaximales xs) es la lista de los subconjuntos maximales
-- de xs. Por ejemplo,
--    subconjuntosMaximales [2,5,3]  ==  [[5,3],[2,3],[2,5]]
subconjuntosMaximales :: [Int] -> [[Int]]
subconjuntosMaximales xs =
  [delete x xs | x <- xs]

-- Definición de nOcurrenciasArbolSubconjuntos
-- ===========================================

nOcurrenciasArbolSubconjuntos :: [Int] -> [Int] -> Int
nOcurrenciasArbolSubconjuntos xs ys =
  nOcurrencias xs (arbolSubconjuntos ys)

-- (nOcurrencias x a) es el número de veces que aparece x en el árbol
-- a. Por ejemplo,
--    nOcurrencias 3 (arbolSubconjuntos 30)  ==  2
nOcurrencias :: [Int] -> Arbol -> Int
nOcurrencias xs (N ys [])
  | conjunto xs == conjunto ys  = 1
  | otherwise                   = 0
nOcurrencias xs (N ys zs)
  | conjunto xs == conjunto ys = 1 + sum [nOcurrencias xs z | z <- zs]
  | otherwise                  = sum [nOcurrencias xs z | z <- zs]

-- (conjunto xs) es el conjunto ordenado correspondiente a xs. Por
-- ejemplo, 
--    conjunto [3,2,5,2,3,7,2]  ==  [2,3,5,7]
conjunto :: [Int] -> [Int]
conjunto = nub . sort

-- La propiedad es
prop_nOcurrencias :: (Positive Int) -> [Int] -> Bool
prop_nOcurrencias (Positive n) xs =
  nOcurrenciasArbolSubconjuntos ys [1..n] == factorial (n-k)
  where ys = nub [1 + x `mod` n | x <- xs]
        k  = length ys
        factorial m = product [1..m]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=9}) prop_nOcurrencias
--    +++ OK, passed 100 tests.
\end{code} 
