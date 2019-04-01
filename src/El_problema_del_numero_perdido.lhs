% El_problema_del_numero_perdido.lhs
% El problema del número perdido.
% José A. Alonso Jiménez
% Sevilla, 25 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¡Reventó de risa! \\
     ¡Un hombre tan serio! \\
     \dots Nadie lo diría.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module El_problema_del_numero_perdido where
\end{code}
\end{comment}

Sea xs una lista de números consecutivos (creciente o decreciente),
en la que puede faltar algún número. El problema del número perdido
en xs consiste en lo siguiente:
\begin{itemize}
\item si falta un único número z, devolver Just z
\item si no falta ninguno, devolver Nothing
\end{itemize}

Definir la función
\begin{descripcion} 
  numeroPerdido :: [Int] -> Maybe Int
\end{descripcion}   
tal que (numeroPerdido xs) es el resultado del problema del número
perdidio en xs. Por ejemplo,
\begin{descripcion} 
  numeroPerdido [7,6,4,3]                     == Just 5
  numeroPerdido [1,2,4,5,6]                   == Just 3
  numeroPerdido [6,5..3]                      == Nothing
  numeroPerdido [1..6]                        == Nothing
  numeroPerdido ([5..10^6] ++ [10^6+2..10^7]) == Just 1000001
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (tails, sort)
import Data.Maybe (listToMaybe)

-- 1ª solución
numeroPerdido :: [Int] -> Maybe Int
numeroPerdido (x:y:xs)
  | abs (y - x) == 1 = numeroPerdido (y:xs)
  | otherwise        = Just (div (x+y) 2)
numeroPerdido _      = Nothing

-- 2ª solución
numeroPerdido2 :: [Int] -> Maybe Int
numeroPerdido2 xs = aux z (z:zs) 
  where (z:zs) = sort xs
        aux _ [] = Nothing
        aux y (x:xs') | y == x    = aux (y+1) xs'
                      | otherwise = Just y

-- 3ª solución
-- ===========

numeroPerdido3 :: [Int] -> Maybe Int
numeroPerdido3 xs =
  listToMaybe [(a+b) `div` 2 | (a:b:_) <- tails xs, abs(a-b) /= 1]
\end{code} 
