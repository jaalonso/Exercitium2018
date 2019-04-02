% Arboles_cuyas_ramas_cumplen_una_propiedad.lhs
% Árboles cuyas ramas cumplen una propiedad.
% José A. Alonso Jiménez
% Sevilla, 26 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Por dar al viento trabajo, \\
     cosía con hilo doble \\
     las hojas secas del árbol.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Arboles_cuyas_ramas_cumplen_una_propiedad where
\end{code}
\end{comment}

Los árboles se pueden representar mediante el siguiente tipo de dato
\begin{descripcion} 
  data Arbol a = N a [Arbol a]
    deriving Show
\end{descripcion}   
Por ejemplo, los árboles
\begin{descripcion} 
      -1           1            1
      / \         / \          /|\
     2   3      -2   3        / | \  
    / \          |          -2  7  3  
   4   5        -4          / \      
                           4   5     
\end{descripcion} 
se representan por
\begin{descripcion} 
  ej1, ej2, ej3 :: Arbol Int
  ej1 = N (-1) [N 2 [N 4 [], N 5 []], N 3 []]
  ej2 = N 1 [N (-2) [N (-4) []], N 3 []]
  ej3 = N 1 [N (-2) [N 4 [], N 5 []], N 7 [], N 3 []]
\end{descripcion}
 
Definir la función
\begin{descripcion} 
  todasDesdeAlguno :: (a -> Bool) -> Arbol a -> Bool
\end{descripcion}   
tal que (todasDesdeAlguno p ar) se verifica si para toda rama existe un
elemento a partir del cual todos los elementos de la rama verifican
la propiedad p. Por ejemplo,
\begin{descripcion} 
  todasDesdeAlguno (>0) ej1 == True
  todasDesdeAlguno (>0) ej2 == False
  todasDesdeAlguno (>0) ej3 == True
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (tails)

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2, ej3 :: Arbol Int
ej1 = N (-1) [N 2 [N 4 [], N 5 []], N 3 []]
ej2 = N 1 [N (-2) [N (-4) []], N 3 []]
ej3 = N 1 [N (-2) [N 4 [], N 5 []], N 7 [], N 3 []]

-- 1ª solución
-- ===========

todasDesdeAlguno :: (b -> Bool) -> Arbol b -> Bool
todasDesdeAlguno p a = all (desdeAlguno p) (ramas a)

-- (desdeAlguno p xs) se verifica si la propiedad xs tiene un elementemo
-- a partir del cual todos los siguientes cumplen la propiedad p. Por
-- ejemplo, 
--    desdeAlguno (>0) [-1,2,4]   ==  True
--    desdeAlguno (>0) [1,-2,-4]  ==  False
--    desdeAlguno (>0) [1,-2,4]   ==  True

-- 1ª definición de desdeAlguno
desdeAlguno1 :: (a -> Bool) -> [a] -> Bool
desdeAlguno1 p xs =
  not (null (takeWhile p (reverse xs)))

-- 2ª definición de desdeAlguno
desdeAlguno2 :: (a -> Bool) -> [a] -> Bool
desdeAlguno2 p xs = any (all p) (init (tails xs))

-- Comparación de eficiencia:
--    λ> desdeAlguno1 (>10^7) [1..1+10^7]
--    True
--    (4.36 secs, 960,101,896 bytes)
--    λ> desdeAlguno2 (>10^7) [1..1+10^7]
--    True
--    (5.62 secs, 3,600,101,424 bytes)

-- Usaremos la 1ª definición de desdeAlguno
desdeAlguno :: (a -> Bool) -> [a] -> Bool
desdeAlguno = desdeAlguno1

-- (ramas a) es la lista de las ramas de a. Por ejemplo,
--    ramas ej1  ==  [[-1,2,4],[-1,2,5],[-1,3]]
--    ramas ej2  ==  [[1,-2,-4],[1,3]]
--    ramas ej3  ==  [[1,-2,4],[1,-2,5],[1,7],[1,3]]
ramas :: Arbol a -> [[a]]
ramas (N x []) = [[x]]
ramas (N x as) = map (x:) (concatMap ramas as)

-- 2ª solución
-- ===========

todasDesdeAlguno2 :: (b -> Bool) -> Arbol b -> Bool
todasDesdeAlguno2 p (N x []) = p x
todasDesdeAlguno2 p (N _ as) = all (todasDesdeAlguno2 p) as 
\end{code} 
