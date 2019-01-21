% Ofertas_3_por_2.lhs
% Ofertas 3 por 2.
% José A. Alonso Jiménez
% Sevilla, 14 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Despacito y buena letra: \\
     el hacer las cosas bien \\
     importa más que el hacerlas.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Ofertas_3_por_2 where
\end{code}
\end{comment}

En una tienda tiene la ``oferta 3 por 2'' de forma que cada cliente que
elige 3 artículos obtiene el más barato de forma gratuita. Por
ejemplo, si los precios de los artículos elegidos por un cliente son
10, 2, 4, 5 euros pagará 19 euros si agrupa los artículos en (10,2,4)
y (5) o pagará 17 si lo agrupa en (5,10,4) y (2). 

Definir la función
\begin{descripcion} 
  minimoConOferta :: [Int] -> Int
\end{descripcion} 
tal que (minimoConOferta xs) es lo mínimo que pagará el cliente si
los precios de la compra son xs; es decir, lo que pagará agrupando
los artículos de forma óptima para aplicar la oferta 3 por 2. Por
ejemplo,
\begin{descripcion} 
  minimoConOferta [10,2,4,5]     ==  17
  minimoConOferta [3,2,3,2]      ==  8
  minimoConOferta [6,4,5,5,5,5]  ==  21
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.List (sort, sortOn)
import Data.Ord  (Down(..))

-- 1ª solución
-- ===========

minimoConOferta :: [Int] -> Int
minimoConOferta xs = sum (sinTerceros (reverse (sort xs)))

sinTerceros :: [a] -> [a]
sinTerceros []         = []
sinTerceros [x]        = [x]
sinTerceros [x,y]      = [x,y]
sinTerceros (x:y:_:zs) = x : y : sinTerceros zs

-- 2ª solución
-- ===========

minimoConOferta2 :: [Int] -> Int
minimoConOferta2 = sum . sinTerceros . reverse . sort

-- 3ª solución
-- ===========

minimoConOferta3 :: [Int] -> Int
minimoConOferta3 = sum . sinTerceros . sortOn Down

-- 4ª solución
-- ===========

minimoConOferta4 :: [Int] -> Int
minimoConOferta4 xs = aux (reverse (sort xs)) 0
  where aux (a:b:_:ds) n = aux ds (a+b+n)
        aux as         n = n + sum as 

-- 5ª solución
-- ===========

minimoConOferta5 :: [Int] -> Int
minimoConOferta5 xs = aux (sortOn Down xs) 0
  where aux (a:b:_:ds) n = aux ds (a+b+n)
        aux as         n = n + sum as 
\end{code} 
