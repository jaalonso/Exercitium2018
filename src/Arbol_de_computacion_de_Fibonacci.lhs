% Arbol_de_computacion_de_Fibonacci.lhs
% Árbol de computación de Fibonacci.
% José A. Alonso Jiménez
% Sevilla, 11 de Diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Arbol_de_computacion_de_Fibonacci where    
\end{code}
\end{comment}

La sucesión de Fibonacci es
\begin{descripcion}
   0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,...
\end{descripcion}
cuyos dos primeros términos son 0 y 1 y los restantentes se obtienen
sumando los dos anteriores.

El árbol de computación de su 5º término es
\begin{descripcion}
                 5
                / \
               /   \
              /     \
             /       \
            /         \
           3           2  
          / \         / \ 
         /   \       1   1
        2     1     / \   
       / \   / \   1   0  
      1   1 1   0
     / \ 
    1   0  
\end{descripcion}
que, usando los árboles definidos por
\begin{descripcion}
   data Arbol = H Int
              | N Int Arbol Arbol
     deriving (Eq, Show)
\end{descripcion}
se puede representar por
\begin{descripcion}
   N 5              
     (N 3           
        (N 2        
           (N 1 (H 1) (H 0))
           (H 1))   
        (N 1 (H 1) (H 0)))  
     (N 2           
        (N 1 (H 1) (H 0))   
        (H 1))     
\end{descripcion}
      
Definir las funciones
\begin{descripcion}
   arbolFib           :: Int -> Arbol
   nElementosArbolFib :: Int -> Int
\end{descripcion}
tales que
\begin{itemize}
\item (arbolFib n) es el árbol de computación del n--ésimo término de la
  sucesión de Fibonacci. Por ejemplo,
\begin{descripcion}  
     λ> arbolFib 5
     N 5              
       (N 3           
          (N 2        
             (N 1 (H 1) (H 0))
             (H 1))   
          (N 1 (H 1) (H 0)))  
       (N 2           
          (N 1 (H 1) (H 0))   
          (H 1))
     λ> arbolFib 6
     N 8
       (N 5
          (N 3
             (N 2
                (N 1 (H 1) (H 0))
                (H 1))
             (N 1 (H 1) (H 0)))
          (N 2
             (N 1 (H 1) (H 0))
             (H 1)))
       (N 3
          (N 2
             (N 1 (H 1) (H 0)) (H 1))
          (N 1 (H 1) (H 0)))
\end{descripcion}
\item (nElementosArbolFib n) es el número de elementos en el árbol de
  computación del n-ésimo término de la sucesión de Fibonacci. Por
  ejemplo,
\begin{descripcion}
     nElementosArbolFib 5   ==  15
     nElementosArbolFib 6   ==  25
     nElementosArbolFib 30  ==  2692537
\end{descripcion}
\end{itemize}

\section*{Soluciones}

\begin{code}
data Arbol = H Int
           | N Int Arbol Arbol
  deriving (Eq, Show)

-- 1ª definición
-- =============

arbolFib :: Int -> Arbol
arbolFib 0 = H 0
arbolFib 1 = H 1
arbolFib n = N (fib n) (arbolFib (n-1)) (arbolFib (n-2))

-- (fib n) es el n-ésimo elemento de la sucesión de Fibonacci. Por
-- ejemplo,
--    fib 5  ==  5
--    fib 6  ==  8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 2ª definición
-- =============

arbolFib2 :: Int -> Arbol
arbolFib2 0 = H 0
arbolFib2 1 = H 1
arbolFib2 2 = N 1 (H 1) (H 0)
arbolFib2 3 = N 2 (N 1 (H 1) (H 0)) (H 1)
arbolFib2 n = N (a1 + a2) (N a1 i1 d1) (N a2 i2 d2)
  where (N a1 i1 d1) = arbolFib2 (n-1)
        (N a2 i2 d2) = arbolFib2 (n-2)

-- 3ª definición
-- =============

arbolFib3 :: Int -> Arbol
arbolFib3 0 = H 0
arbolFib3 1 = H 1
arbolFib3 2 = N 1 (H 1) (H 0)
arbolFib3 3 = N 2 (N 1 (H 1) (H 0)) (H 1)
arbolFib3 n = N (a + b) i d
  where i@(N a _ _) = arbolFib3 (n-1)
        d@(N b _ _) = arbolFib3 (n-2)

-- 1ª definición de nElementosArbolFib
-- ===================================

nElementosArbolFib :: Int -> Int
nElementosArbolFib = length . elementos . arbolFib3

-- (elementos a) es la lista de elementos del árbol a. Por ejemplo,
--    λ> elementos (arbolFib 5)
--    [5,3,2,1,1,0,1,1,1,0,2,1,1,0,1]
--    λ> elementos (arbolFib 6)
--    [8,5,3,2,1,1,0,1,1,1,0,2,1,1,0,1,3,2,1,1,0,1,1,1,0]
elementos :: Arbol -> [Int]
elementos (H x)     = [x]
elementos (N x i d) = x : elementos i ++ elementos d

-- 2ª definición de nElementosArbolFib
-- ===================================

nElementosArbolFib2 :: Int -> Int
nElementosArbolFib2 0 = 1
nElementosArbolFib2 1 = 1
nElementosArbolFib2 n = 1 + nElementosArbolFib2 (n-1)
                          + nElementosArbolFib2 (n-2)
\end{code}
