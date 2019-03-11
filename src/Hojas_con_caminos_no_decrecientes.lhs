% Hojas_con_caminos_no_decrecientes.lhs
% Hojas con caminos no decrecientes.
% José A. Alonso Jiménez
% Sevilla, 4 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Para dialogar, \\
     preguntad, primero; \\
     después \dots escuchad.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Hojas_con_caminos_no_decrecientes where
\end{code}
\end{comment}

Los árboles se pueden representar mediante el siguiente tipo de datos
\begin{descripcion} 
  data Arbol = N Int [Arbol]
    deriving Show
\end{descripcion} 
Por ejemplo, los árboles
\begin{descripcion} 
         1             1             1  
        /  \          / \           / \ 
       /    \        8   3         8   3
      2      6          /|\       /|\  |
     / \    / \        4 2 6     4 5 6 2
    4   5  5   7
\end{descripcion} 
se representan por
\begin{descripcion} 
  ej1, ej2, ej3 :: Arbol
  ej1 = N 1 [N 2 [N 4 [], N 5 []], N 6 [N 5 [], N 7 []]]
  ej2 = N 1 [N 8 [], N 3 [N 4 [], N 2 [], N 6 []]]
  ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 2 []]]
\end{descripcion}

Definir la función
\begin{descripcion} 
  hojasEnNoDecreciente :: Arbol -> [Int]
\end{descripcion} 
tal que (hojasEnNoDecreciente a) es el conjunto de las hojas de a que
se encuentran en alguna rama no decreciente. Por ejemplo,
\begin{descripcion} 
  hojasEnNoDecreciente ej1  ==  [4,5,7]
  hojasEnNoDecreciente ej2  ==  [4,6,8]
  hojasEnNoDecreciente ej3  ==  []
\end{descripcion}

\section*{Soluciones}

\begin{code}    
import Data.List (sort, nub)

data Arbol = N Int [Arbol]
  deriving Show
           
ej1, ej2, ej3 :: Arbol
ej1 = N 1 [N 2 [N 4 [], N 5 []], N 6 [N 5 [], N 7 []]]
ej2 = N 1 [N 8 [], N 3 [N 4 [], N 2 [], N 6 []]]
ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 2 []]]

-- 1ª solución
-- ===========

hojasEnNoDecreciente :: Arbol -> [Int]
hojasEnNoDecreciente a =
  sort (nub (map last (ramasNoDecrecientes a)))

--    ramasNoDecrecientes ej1  ==  [[1,2,4],[1,2,5],[1,6,7]]
--    ramasNoDecrecientes ej2  ==  [[1,8],[1,3,4],[1,3,6]]
--    ramasNoDecrecientes ej3  ==  []
ramasNoDecrecientes :: Arbol -> [[Int]]
ramasNoDecrecientes a =
  filter esNoDecreciente (ramas a)

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    λ> ramas ej1
--    [[1,2,4],[1,2,5],[1,6,5],[1,6,7]]
--    λ> ramas ej2
--    [[1,8],[1,3,4],[1,3,2],[1,3,6]]
--    λ> ramas ej3
--    [[1,8,4],[1,8,5],[1,8,6],[1,3,2]]
ramas :: Arbol -> [[Int]]
ramas (N x []) = [[x]]
ramas (N x as) = map (x:) (concatMap ramas as)

-- (esNoDecreciente xs) se verifica si la lista xs es no
-- decreciente. Por ejemplo, 
--    esNoDecreciente [1,3,3,5]  ==  True
--    esNoDecreciente [1,3,5,3]  ==  False
esNoDecreciente :: [Int] -> Bool
esNoDecreciente xs =
  and (zipWith (<=) xs (tail xs))

-- 2ª solución
-- ===========

--    hojasEnNoDecreciente ej1  ==  [4,5,7]
--    hojasEnNoDecreciente ej2  ==  [4,6,8]
--    hojasEnNoDecreciente ej3  ==  []
hojasEnNoDecreciente2 :: Arbol -> [Int]
hojasEnNoDecreciente2 = sort . nub . aux
  where
    aux (N x []) = [x]
    aux (N x as) = concat [aux (N y bs) | (N y bs) <- as, x <= y]
\end{code} 
