% Siguiente_mayor.lhs
% Siguiente mayor.
% José A. Alonso Jiménez
% Sevilla, 20 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Si vivir es bueno \\
     es mejor soñar, \\
     y mejor que todo, \\
     madre, despertar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Siguiente_mayor where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  siguienteMayor :: Ord a => [a] -> [Maybe a]
\end{descripcion}   
tal que (siguienteMayos xs) es la lista obtenida sustiyendo cada
elemento de xs por el primer elemento de xs a la derechha de x que
sea mayor que x, si existe y Nothing en caso contrario. Por ejemplo,
\begin{descripcion} 
  λ> siguienteMayor [4,5,2,3,9]
  [Just 5,Just 9,Just 3,Just 9,Nothing]
  λ> siguienteMayor [9,5,2,3,4]
  [Nothing,Nothing,Just 3,Just 4,Nothing]
  λ> siguienteMayor [9,5,2,2,4]
  [Nothing,Nothing,Just 4,Just 4,Nothing]
  λ> siguienteMayor "betis"
  [Just 'e',Just 't',Nothing,Just 's',Nothing]
  λ> siguienteMayor "sevilla"
  [Just 'v',Just 'v',Nothing,Just 'l',Nothing,Nothing,Nothing]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.Maybe (listToMaybe)

-- 1ª solución
siguienteMayor :: Ord a => [a] -> [Maybe a]
siguienteMayor [] = []
siguienteMayor (x:xs)
  | null ys   = Nothing : siguienteMayor xs
  | otherwise = Just (head ys) : siguienteMayor xs
  where ys = [y | y <- xs, y > x]

-- 2ª solución
siguienteMayor2 :: Ord a => [a] -> [Maybe a]
siguienteMayor2 []     = []
siguienteMayor2 (x:xs) = listToMaybe [y | y <- xs, y > x] : siguienteMayor2 xs

-- 3ª solución
siguienteMayor3 :: Ord a => [a] -> [Maybe a]
siguienteMayor3 []     = []
siguienteMayor3 (x:xs) = listToMaybe (dropWhile (<=x) xs) : siguienteMayor3 xs
\end{code} 
