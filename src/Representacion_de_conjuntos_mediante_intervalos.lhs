% Representacion_de_conjuntos_mediante_intervalos.lhs
% Representación de conjuntos mediante intervalos.
% José A. Alonso Jiménez
% Sevilla, 15 de enero de 2019
% ---------------------------------------------------------------------

\epigraph {\textit{Cuando el saber se especializa, crece el volumen
    total de la cultura. Esta es la ilusión y el consuelo de los
    especialistas. ¡Lo que sabemos entre todos! ¡Oh, eso es lo que no
    sabe nadie!}}  {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Representacion_de_conjuntos_mediante_intervalos where
\end{code}
\end{comment}

Un conjunto de números enteros se pueden representar mediante una
lista ordenada de intervalos tales que la diferencia entre el menor
elemento de un intervalo y el mayor elemento de su intervalo anterior
es mayor que uno.

Por ejemplo, el conjunto \{2, 7, 4, 3, 9, 6\} se puede representar
mediante la lista de intervalos [(2,4),(6,7),(9,9)] de forma que en
el primer intervalo se agrupan los números 2, 3 y 4; en el segundo,
los números 6 y 7 y el tercero, el número 9.

Definir la función
\begin{descripcion} 
  intervalos :: [Int] -> [(Int,Int)]
\end{descripcion} 
tal que (intervalos xs) es lista ordenada de intervalos que
representa al conjunto xs. Por ejemplo,
\begin{descripcion} 
  λ> intervalos [2,7,4,3,9,6]
  [(2,4),(6,7),(9,9)]
  λ> intervalos [180,141,174,143,142,175]
  [(141,143),(174,175),(180,180)]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (sort)

intervalos :: [Int] -> [(Int,Int)]
intervalos = map intervalo . segmentos

-- (segmentos xs) es la lista de segmentos formados por elementos
-- consecutivos de xs. Por ejemplo,
--    segmentos [2,7,4,3,9,6]  ==  [[2,3,4],[6,7],[9]]
segmentos :: [Int] -> [[Int]]
segmentos xs = aux bs [[b]]
  where aux [] zs = zs
        aux (y:ys) ((a:as):zs) | y == a-1  = aux ys ((y:a:as):zs)
                               | otherwise = aux ys ([y]:(a:as):zs)
        (b:bs) = reverse (sort xs)

-- (intervalo xs) es el intervalo correspondiente al segmento xs. Por
-- ejemplo, 
--    intervalo [2,3,4]  ==  (2,4)
--    intervalo [6,7]    ==  (6,7)
--    intervalo [9]      ==  (9,9)
intervalo :: [Int] -> (Int,Int)
intervalo xs = (head xs, last xs)
\end{code} 
