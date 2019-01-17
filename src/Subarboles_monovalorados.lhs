% Subarboles_monovalorados.lhs
% Subárboles monovalorados
% José A. Alonso Jiménez
% Sevilla, 19 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Y nadie pregunta \\
          ni nadie contesta, \\
          todos hablan solos.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Subarboles_monovalorados where
\end{code}
\end{comment}

Los árboles binarios con valores enteros se pueden representar
mediante el tipo Arbol definido por
\begin{descripcion} 
  data Arbol = H Int 
             | N Int Arbol Arbol
             deriving Show
\end{descripcion}
           
Por ejemplo, el árbol
\begin{descripcion} 
         7
        / \ 
       /   \
      /     \
     4       9
    / \     / \
   1   3   5   6 
\end{descripcion} 
se puede representar por
\begin{descripcion} 
  N 7 (N 4 (H 1) (H 3)) (N 9 (H 5) (H 6))
\end{descripcion}

Un árbol es monovalorado si todos sus elementos son iguales. Por
ejemplo, de los siguientes árboles sólo son monovalorados los dos
primeros
\begin{descripcion} 
    5          9           5          9    
   / \        / \         / \        / \   
  5   5      9   9       5   6      9   7  
                / \                    / \ 
               9   9                  9   9
\end{descripcion} 

Definir la función
\begin{descripcion} 
  monovalorados :: Arbol -> [Arbol]
\end{descripcion} 
tal que (monovalorados a) es la lista de los subárboles monovalorados
de a. Por ejemplo,
\begin{descripcion} 
  λ> monovalorados (N 5 (H 5) (H 5))
  [N 5 (H 5) (H 5),H 5,H 5]
  λ> monovalorados (N 5 (H 5) (H 6))
  [H 5,H 6]
  λ> monovalorados (N 9 (H 9) (N 9 (H 9) (H 9)))
  [N 9 (H 9) (N 9 (H 9) (H 9)),H 9,N 9 (H 9) (H 9),H 9,H 9]
  λ> monovalorados (N 9 (H 9) (N 7 (H 9) (H 9)))
  [H 9,H 9,H 9]
  λ> monovalorados (N 9 (H 9) (N 9 (H 7) (H 9)))
  [H 9,H 7,H 9]
\end{descripcion}

\section*{Soluciones}

\begin{code}    
data Arbol = H Int 
           | N Int Arbol Arbol
           deriving (Show, Eq)

monovalorados :: Arbol -> [Arbol]
monovalorados (H x) = [H x]
monovalorados (N x i d) 
    | todosIguales i x && todosIguales d x =
        N x i d : subarboles i ++ subarboles d
    | otherwise = monovalorados i ++ monovalorados d

-- (todosIguales a x) se verifica si todos los valores de los nodos y
-- las hojas del árbol a son iguales a x.
todosIguales :: Arbol -> Int -> Bool
todosIguales (H y) x     = y == x
todosIguales (N y i d) x = y == x && todosIguales i x && todosIguales d x

-- (subarboles a) es la lista de los subárboles de a.
subarboles :: Arbol -> [Arbol]
subarboles (H x)     = [H x]
subarboles (N x i d) = N x i d : subarboles i ++ subarboles d
\end{code} 
