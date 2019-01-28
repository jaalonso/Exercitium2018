% Interseccion_de_listas_infinitas_crecientes.lhs
% Intersección de listas infinitas crecientes.
% José A. Alonso Jiménez
% Sevilla, 21 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Alguna vez he pensado \\
     si el alma será la ausencia, \\
     mientras más cerca más lejos; \\
     mientras más lejos más cerca.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Interseccion_de_listas_infinitas_crecientes where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  interseccion :: Ord a => [[a]] -> [a]
\end{descripcion} 
tal que (interseccion xss) es la intersección de la lista no vacía de
listas infinitas crecientes xss; es decir, la lista de los elementos
que pertenecen a todas las listas de xss. Por ejemplo,
\begin{descripcion} 
   λ> take 10 (interseccion [[2,4..],[3,6..],[5,10..]])
   [30,60,90,120,150,180,210,240,270,300]
   λ> take 10 (interseccion [[2,5..],[3,5..],[5,7..]])
   [5,11,17,23,29,35,41,47,53,59]
 \end{descripcion}
 
\section*{Soluciones}

\begin{code} 
-- 1ª solución
-- ===========

interseccion :: Ord a => [[a]] -> [a]
interseccion [xs]        = xs
interseccion (xs:ys:zss) = interseccionDos xs (interseccion (ys:zss))

interseccionDos :: Ord a => [a] -> [a] -> [a]
interseccionDos (x:xs) (y:ys)
  | x == y    = x : interseccionDos xs ys
  | x < y     = interseccionDos (dropWhile (<y) xs) (y:ys)
  | otherwise = interseccionDos (x:xs) (dropWhile (<x) ys)  

-- 2ª solución
-- ===========

interseccion2 :: Ord a => [[a]] -> [a]
interseccion2 = foldl1 interseccionDos

-- 3ª solución
-- ===========

interseccion3 :: Ord a => [[a]] -> [a]
interseccion3 (xs:xss) =
  [x | x <- xs, all (x `pertenece`) xss]
 
pertenece :: Ord a => a -> [a] -> Bool
pertenece x xs = x == head (dropWhile (<x) xs)
\end{code} 
