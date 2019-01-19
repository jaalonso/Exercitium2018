% Superacion_de_limites.lhs
% Superación de límites.
% José A. Alonso Jiménez 
% Sevilla, 14 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Todo necio confunde valor y precio.}} {Antonio
  Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Superacion_de_limites where
\end{code}
\end{comment}

Una sucesión de puntuaciones se puede representar mediante una lista
de números. Por ejemplo, [7,5,9,9,4,5,4,2,5,9,12,1]. En la lista
anterior, los puntos en donde se alcanzan un nuevo máximo son
7, 9 y 12 (porque son mayores que todos sus anteriores) y en donde se
alcanzan un nuevo mínimo son 7, 5, 4, 2 y 1 (porque son menores que
todos sus anteriores). Por tanto, el máximo se ha superado 2 veces y
el mínimo 4 veces.

Definir las funciones
\begin{descripcion}
  nuevosMaximos :: [Int] -> [Int]
  nuevosMinimos :: [Int] -> [Int]
  nRupturas     :: [Int] -> (Int,Int)
\end{descripcion}
tales que
\begin{itemize}
\item (nuevosMaximos xs) es la lista de los nuevos máximos de xs. Por
  ejemplo,
\begin{descripcion}  
  nuevosMaximos [7,5,9,9,4,5,4,2,5,9,12,1]  ==  [7,9,12]
\end{descripcion}
\item (nuevosMinimos xs) es la lista de los nuevos mínimos de xs. Por
  ejemplo,
\begin{descripcion}  
  nuevosMinimos [7,5,9,9,4,5,4,2,5,9,12,1]  ==  [7,5,4,2,1]
\end{descripcion}
\item (nRupturas xs) es el par formado por el número de veces que se
  supera el máximo y el número de veces que se supera el mínimo en
  xs. Por ejemplo,
\begin{descripcion}  
  nRupturas [7,5,9,9,4,5,4,2,5,9,12,1]  ==  (2,4)
\end{descripcion}
\end{itemize}

\section*{Soluciones}

\begin{code}
import Data.List (group, inits)

nuevosMaximos :: [Int] -> [Int]
nuevosMaximos xs = map head (group (map maximum xss))
  where xss = tail (inits xs)

nuevosMinimos :: [Int] -> [Int]
nuevosMinimos xs = map head (group (map minimum xss))
  where xss = tail (inits xs)

nRupturas :: [Int] -> (Int,Int)
nRupturas [] = (0,0)
nRupturas xs =
  ( length (nuevosMaximos xs) - 1
  , length (nuevosMinimos xs) - 1)
\end{code}
