% Cambio_con_el_menor_numero_de_monedas.lhs
% Cambio con el menor número de monedas.
% José A. Alonso Jiménez
% Sevilla, 15 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Demos tiempo al tiempo: \\
     para que el vaso rebose \\
     hay que llenarlo primero.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Cambio_con_el_menor_numero_de_monedas where
\end{code}
\end{comment}

El problema del cambio con el menor número de monedas consiste en,
dada una lista ms de tipos de monedas (con infinitas monedas de cada
tipo) y una cantidad objetivo x, calcular el menor número de monedas
de ms cuya suma es x. Por ejemplo, con monedas de 1, 3 y 4 céntimos
se puede obtener 6 céntimos de 4 formas
\begin{descripcion} 
  1, 1, 1, 1, 1, 1
  1, 1, 1, 3
  1, 1, 4
  3, 3
\end{descripcion}    
El menor número de monedas que se necesita es 2. En cambio, con
monedas de 2, 5 y 10 es imposible obtener 3.

Definir
\begin{descripcion} 
  monedas :: [Int] -> Int -> Maybe Int
\end{descripcion}   
tal que (monedas ms x) es el menor número de monedas de ms cuya suma
es x, si es posible obtener dicha suma y es Nothing en caso
contrario. Por ejemplo,
\begin{descripcion} 
  monedas [1,3,4]  q                    ==  Just 2
  monedas [2,5,10] 3                    ==  Nothing
  monedas [1,2,5,10,20,50,100,200] 520  ==  Just 4
\end{descripcion}

\section*{Soluciones}

\begin{code}    
import Data.Array ((!), array)

-- 1ª solución
-- ===========

monedas :: [Int] -> Int -> Maybe Int
monedas ms x
  | null cs   = Nothing
  | otherwise = Just (minimum (map length cs))
  where cs = cambios ms x

-- (cambios ms x) es la lista de las foemas de obtener x sumando monedas
-- de ms. Por ejemplo,
--   λ> cambios [1,5,10] 12
--   [[1,1,1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,5],[1,1,5,5],[1,1,10]]
--   λ> cambios [2,5,10] 3
--   []
--   λ> cambios [1,3,4] 6
--   [[1,1,1,1,1,1],[1,1,1,3],[1,1,4],[3,3]]
cambios :: [Int] -> Int -> [[Int]]
cambios _      0 = [[]]
cambios []     _ = []
cambios (k:ks) m
  | m < k     = []
  | otherwise = [k:zs | zs <- cambios (k:ks) (m - k)] ++
                cambios ks m

-- 2ª solución
-- ===========

monedas2 :: [Int] -> Int -> Maybe Int
monedas2 ms n
  | sol == infinito = Nothing
  | otherwise       = Just sol
  where
    sol = aux n
    aux 0 = 0
    aux k = siguiente (minimo [aux (k - x) | x <- ms,  k >= x])

infinito :: Int
infinito = 10^30

minimo :: [Int] -> Int
minimo [] = infinito
minimo xs = minimum xs

siguiente :: Int -> Int
siguiente x | x == infinito = infinito
            | otherwise     = 1 + x

-- 3ª solución
-- ===========

monedas3 :: [Int] -> Int -> Maybe Int
monedas3 ms n  
  | sol == infinito = Nothing
  | otherwise       = Just sol
  where
    sol = v ! n
    v   = array (0,n) [(i,f i) | i <- [0..n]]
    f 0 = 0
    f k = siguiente (minimo [v ! (k - x) | x <- ms, k >= x])

-- Comparación de eficiencia
-- =========================

--    λ> monedas [1,2,5,10,20,50,100,200] 27
--    Just 3
--    (0.02 secs, 871,144 bytes)
--    λ> monedas2 [1,2,5,10,20,50,100,200] 27
--    Just 3
--    (15.44 secs, 1,866,519,080 bytes)
--    λ> monedas3 [1,2,5,10,20,50,100,200] 27
--    Just 3
--    (0.01 secs, 157,232 bytes)
--    
--    λ> monedas [1,2,5,10,20,50,100,200] 188
--    Just 7
--    (14.20 secs, 1,845,293,080 bytes)
--    λ> monedas3 [1,2,5,10,20,50,100,200] 188
--    Just 7
--    (0.01 secs, 623,376 bytes)
\end{code} 
