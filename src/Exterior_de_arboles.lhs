% Exterior_de_arboles.lhs
% Exterior de árboles.
% José A. Alonso Jiménez
% Sevilla, 6 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Dónde está la utilidad \\
     de nuestras utilidades? \\
     Volvamos a la verdad: \\
     vanidad de vanidades.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Exterior_de_arboles where
\end{code}
\end{comment}

Los árboles binarios con datos en las hojas y los nodos se definen
por
\begin{descripcion} 
  data Arbol = H Int
             | N Int Arbol Arbol 
    deriving Show
\end{descripcion} 
Por ejemplo, los árboles
\begin{descripcion} 
         3               3               3     
        / \             / \             / \    
       /   \           /   \           /   \   
      2     8         2     8         2     8  
     / \   / \       / \   / \       / \   / \ 
    5   7 6   9     5   7 6   9     5   7 6   9
   / \                   / \                 / \   
  1   4                 1   4               1   4  
\end{descripcion} 
se representan por
\begin{descripcion} 
  ejArbol1, ejArbol2, ejArbol3 :: Arbol 
  ejArbol1 = N 3
               (N 2 
                  (N 5 (H 1) (H 4))
                  (H 7))
               (N 8 (H 6) (H 9))
  ejArbol2 = N 3
               (N 2 (H 5) (H 7))
               (N 8 (N 6 (H 1) (H 4))
                    (H 9))
  ejArbol3 = N 3
               (N 2 (H 5) (H 7))
               (N 8 (H 6)
                    (N 9 (H 1) (H 4)))
\end{descripcion}
                  
Definir la función
\begin{descripcion} 
  exterior :: Arbol -> [Int]
\end{descripcion}   
tal que (exterior a) es la lista de los elementos exteriores del
árbol a. Por ejemplo,
\begin{descripcion} 
  exterior ejArbol1  ==  [3,2,5,1,4,7,6,9,8]
  exterior ejArbol2  ==  [3,2,5,7,1,4,9,8]
  exterior ejArbol3  ==  [3,2,5,7,6,1,4,9,8]
\end{descripcion}

El orden de los elementos es desde la raíz hasta el extremo inferior
izquierdo desde él hasta el inferior derecho y desde él hasta la
raíz.

\section*{Soluciones}

\begin{code} 
data Arbol = H Int
           | N Int Arbol Arbol 
  deriving (Show, Eq)

ejArbol1, ejArbol2, ejArbol3 :: Arbol 
ejArbol1 = N 3
             (N 2 
                (N 5 (H 1) (H 4))
                (H 7))
             (N 8 (H 6) (H 9))
ejArbol2 = N 3
             (N 2 (H 5) (H 7))
             (N 8 (N 6 (H 1) (H 4))
                  (H 9))
ejArbol3 = N 3
             (N 2 (H 5) (H 7))
             (N 8 (H 6)
                  (N 9 (H 1) (H 4)))

exterior :: Arbol -> [Int]
exterior (H x) = [x]
exterior a =
  ramaIzquierda a ++ hojas a ++ reverse (tail (ramaDerecha a))

-- (ramaIzquierda a) es la rama izquierda del árbol a. Por ejemplo,
--    ramaIzquierda ejArbol1  ==  [3,2,5]
--    ramaIzquierda ejArbol3  ==  [3,2]
ramaIzquierda :: Arbol -> [Int]
ramaIzquierda (H _)     = []
ramaIzquierda (N x i _) = x : ramaIzquierda i

-- (ramaDerecha a) es la rama derecha del árbol a. Por ejemplo,
--    ramaDerecha ejArbol1  ==  [3,8]
--    ramaDerecha ejArbol3  ==  [3,8,9]
ramaDerecha :: Arbol -> [Int]
ramaDerecha (H _)     = []
ramaDerecha (N x _ d) = x : ramaDerecha d

-- (hojas a) es la lista de las hojas del árbol a. Por ejemplo,
--    hojas ejArbol1  ==  [1,4,7,6,9]
--    hojas ejArbol3  ==  [5,7,6,1,4]
hojas :: Arbol -> [Int]
hojas (H x)     = [x]
hojas (N _ i d) = hojas i ++ hojas d
\end{code} 
