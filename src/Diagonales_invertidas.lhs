% Diagonales_invertidas.lhs
% Diagonales invertidas.
% José A. Alonso Jiménez
% Sevilla, 13 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Despertad, cantores: \\
     acaben los ecos, \\
     empiecen las voces.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Diagonales_invertidas where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  diagonalesInvertidas :: Matrix a -> Matrix a
\end{descripcion}   
tal que (diagonalesInvertidas q) es la matriz obtenida invirtiendo el
orden de los elementos de la diagonal principal y de la diagonal
secundaria de q. Por ejemplo,
\begin{descripcion} 
   λ> fromList 5 5 [1..]
   |  1  2  3  4  5 |
   |  6  7  8  9 10 |
   | 11 12 13 14 15 |
   | 16 17 18 19 20 |
   | 21 22 23 24 25 |
   λ> diagonalesInvertidas (fromList 5 5 [1..])
   | 25  2  3  4 21 |
   |  6 19  8 17 10 |
   | 11 12 13 14 15 |
   | 16  9 18  7 20 |
   |  5 22 23 24  1 |
   λ> fromList 3 3 ['a','b'..]
   | 'a' 'b' 'c' |
   | 'd' 'e' 'f' |
   | 'g' 'h' 'i' |
   λ> diagonalesInvertidas (fromList 3 3 ['a','b'..])
   | 'i' 'b' 'g' |
   | 'd' 'e' 'f' |
   | 'c' 'h' 'a' |
\end{descripcion}
 
\section*{Soluciones}

\begin{code} 
import Data.Matrix

diagonalesInvertidas :: Matrix a -> Matrix a
diagonalesInvertidas q = matrix n n f
  where n = nrows q
        f (i,j) | i == j     = q ! (n + 1 - i, n + 1 - i)
                | i+j == n+1 = q ! (j,i)
                | otherwise  = q ! (i,j)
\end{code} 
