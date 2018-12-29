% Numeracion_de_arboles_binarios_completos.hs
% Numeración de los árboles binarios completos
% José A. Alonso Jiménez
% Sevilla, 3 de Diciembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Numeracion_de_arboles_binarios_completos where
\end{code}
\end{comment}

\section*{Enunciado}

Un \href{http://bit.ly/2DUr53g}{árbol binario completo} es un árbol
binario que tiene todos los nodos posibles hasta el penúltimo nivel,
y donde los elementos del último nivel están colocados de izquierda a
derecha sin dejar huecos entre ellos.

La numeración de los árboles binarios completos se realiza a partir
de la raíz, recorriendo los niveles de izquierda a derecha. Por
ejemplo,
\begin{descripcion}
                  1
                 /  \
                /    \
               /      \
              2        3
             / \      / \
            4   5    6   7
           / \    
          8   9 
\end{descripcion}

Los árboles binarios se puede representar mediante el siguiente tipo
\begin{descripcion}
   data Arbol = H
              | N Int Arbol Arbol
     deriving (Show, Eq)
\end{descripcion}

Definir la función
\begin{descripcion}
    arbolBinarioCompleto :: Int -> Arbol
\end{descripcion}
tal que (arbolBinarioCompleto n) es el árbol binario completo con n
nodos. Por ejemplo,
\begin{descripcion}
   λ> arbolBinarioCompleto 4
   N 1 (N 2 (N 4 H H) H) (N 3 H H)
   λ> pPrint (arbolBinarioCompleto 9)
   N 1
     (N 2
        (N 4
           (N 8 H H)
           (N 9 H H))
        (N 5 H H))
     (N 3
        (N 6 H H)
        (N 7 H H))
\end{descripcion}

\section*{Soluciones}

\begin{code}
data Arbol = H
           | N Int Arbol Arbol
  deriving (Eq, Show)

arbolBinarioCompleto :: Int -> Arbol
arbolBinarioCompleto n = aux 1
  where aux i | i <= n    = N i (aux (2*i)) (aux (2*i+1))
              | otherwise = H
\end{code}

