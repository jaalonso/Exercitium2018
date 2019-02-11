% Limites_de_sucesiones.lhs
% Límites de sucesiones.
% José A. Alonso Jiménez
% Sevilla, 4 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{De diez cabezas, nueve \\
     embisten y una piensa. \\
     Nunca extrañéis que un bruto \\
     se descuerne luchando por la idea.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Limites_de_sucesiones where
\end{code}
\end{comment}

El límite de una sucesión, con una aproximación a y una amplitud n,
es el primer término x de la sucesión tal que el valor absoluto de x
y cualquiera de sus n siguentes elementos es menor que a. 

Definir la función
\begin{descripcion} 
  limite :: [Double] -> Double -> Int -> Double
\end{descripcion} 
tal que (limite xs a n) es el límite de xs xon aproximación a y
amplitud n. Por ejemplo,
\begin{descripcion} 
  λ> limite [(2*n+1)/(n+5) | n <- [1..]] 0.001 300
  1.993991989319092
  λ> limite [(2*n+1)/(n+5) | n <- [1..]] 1e-6 300
  1.9998260062637745
  λ> limite [(1+1/n)**n | n <- [1..]] 0.001 300
  2.7155953364173175
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (tails)

-- 1ª solución
-- ===========

limite :: [Double] -> Double -> Int -> Double
limite (n:ns) x a
  | abs (n - maximum (take (a-1) ns)) < x = n
  | otherwise                             = limite ns x a

-- 2ª solución
-- ===========

limite2 :: [Double] -> Double -> Int -> Double
limite2 xs a n = 
  head [ x | (x:ys) <- segmentos xs n
       , all (\y ->  abs (y - x) < a) ys]

-- (segmentos xs n) es la lista de los segmentos de la lista infinita xs
-- con n elementos. Por ejemplo,   
--    λ> take 5 (segmentos [1..] 3)
--    [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
segmentos :: [a] -> Int -> [[a]]
segmentos xs n = map (take n) (tails xs)

-- Comparación de eficiencia
-- =========================

--    λ> limite [(1+1/n)**n | n <- [1..]] 1e-8 100
--    2.7182700737511185
--    (2.01 secs, 1,185,073,864 bytes)
--    λ> limite2 [(1+1/n)**n | n <- [1..]] 1e-8 100
--    2.7182700737511185
--    (5.03 secs, 1,044,694,264 bytes)
\end{code} 
