% Minimo_numero_de_operaciones_para_transformar_un_numero_en_otro.lhs
% Mínimo número de operaciones para transformar un número en otro.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 18 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Dijiste media verdad? \\
     Dirán que mientes dos veces \\
     si dices la otra mitad. }}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Minimo_numero_de_operaciones_para_transformar_un_numero_en_otro where
\end{code}
\end{comment}

Se considera el siguiente par de operaciones sobre los números:
\begin{itemize}
\item multiplicar por dos
\item restar uno.
\end{itemize}

Dados dos números x e y se desea calcular el menor número de
operaciones para transformar x en y. Por ejemplo, el menor número de
operaciones para transformar el 4 en 7 es 2:
\begin{descripcion} 
   4 ------> 8 ------> 7
      (*2)      (-1)
\end{descripcion} 
y el menor número de operaciones para transformar 2 en 5 es 4
\begin{descripcion} 
   2 ------> 4 ------> 3 ------> 6 ------> 5
      (*2)      (-1)      (*2)      (-1)
\end{descripcion} 

Definir las siguientes funciones
\begin{descripcion} 
  arbolOp :: Int -> Int -> Arbol
  minNOp  :: Int -> Int -> Int
\end{descripcion} 
tales que
\begin{itemize}
\item (arbolOp x n) es el árbol de profundidad n obtenido aplicándole a x
  las dos operaciones. Por ejemplo,
\begin{descripcion}   
  λ> arbolOp 4 1
  N 4 (H 8) (H 3)
  λ> arbolOp 4 2
  N 4 (N 8 (H 16) (H 7))
      (N 3 (H 6) (H 2))
  λ> arbolOp 2 3
  N 2 (N 4
         (N 8 (H 16) (H 7))
         (N 3 (H 6) (H 2)))
      (N 1
         (N 2 (H 4) (H 1))
         (H 0))
  λ> arbolOp 2 4
  N 2 (N 4 (N 8
              (N 16 (H 32) (H 15))
              (N 7 (H 14) (H 6)))
           (N 3
              (N 6 (H 12) (H 5))
              (N 2 (H 4) (H 1))))
      (N 1 (N 2
              (N 4 (H 8) (H 3))
              (N 1 (H 2) (H 0)))
           (H 0))
\end{descripcion} 
\item (minNOp x y) es el menor número de operaciones necesarias para
  transformar x en y. Por ejemplo,
\begin{descripcion}   
  minNOp 4 7  ==  2
  minNOp 2 5  ==  4
  minNOp 2 2  ==  0
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code}      
data Arbol = H Int
           | N Int Arbol Arbol
  deriving (Show, Eq)

arbolOp :: Int -> Int -> Arbol
arbolOp 0 _ = H 0
arbolOp x 0 = H x
arbolOp x n = N x (arbolOp (2 * x) (n - 1)) (arbolOp (x - 1) (n - 1))

ocurre :: Int -> Arbol -> Bool
ocurre x (H y)     = x == y
ocurre x (N y i d) = x == y || ocurre x i || ocurre x d

minNOp :: Int -> Int -> Int
minNOp x y =
  head [n | n <- [0..]
          , ocurre y (arbolOp x n)]
\end{code} 
