% Diferencia_simetrica.hs
% Diferencia simétrica.
% José A. Alonso Jiménez
% Sevilla, 14 de noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Diferencia_simetrica where    
\end{code}
\end{comment}

\section*{Enunciado}

La \href{http://bit.ly/1Rdcqxs}{diferencia simétrica} de dos conjuntos es
el conjunto cuyos elementos son aquellos que pertenecen a alguno de
los conjuntos iniciales, sin pertenecer a ambos a la vez. Por
ejemplo, la diferencia simétrica de {2,5,3} y {4,2,3,7} es {5,4,7}.

Definir la función
\begin{descripcion}
  diferenciaSimetrica :: Ord a => [a] -> [a] -> [a]
\end{descripcion}
tal que (diferenciaSimetrica xs ys) es la diferencia simétrica de xs
e ys. Por ejemplo,
\begin{descripcion}
  diferenciaSimetrica [2,5,3] [4,2,3,7]    ==  [4,5,7]
  diferenciaSimetrica [2,5,3] [5,2,3]      ==  []
  diferenciaSimetrica [2,5,2] [4,2,3,7]    ==  [3,4,5,7]
  diferenciaSimetrica [2,5,2] [4,2,4,7]    ==  [4,5,7]
  diferenciaSimetrica [2,5,2,4] [4,2,4,7]  ==  [5,7]
\end{descripcion}
 
\section*{Soluciones}   

\begin{code}
import Data.List

-- 1ª definición
diferenciaSimetrica :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys =
  sort (nub ([x | x <- xs, x `notElem` ys] ++ [y | y <- ys, y `notElem` xs]))

-- 2ª definición
diferenciaSimetrica2 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica2 xs ys =
  sort (nub (union xs ys \\ intersect xs ys))

-- 3ª definición
diferenciaSimetrica3 :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica3 xs ys =
  [x | x <- sort (nub (xs ++ ys))
     , x `notElem` xs || x `notElem` ys]
\end{code}
   
