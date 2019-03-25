% Permutacion_de_consecutivos.lhs
% Permutación de elementos consecutivos.
% José A. Alonso Jiménez 
% Sevilla, 18 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Entre el vivir y el soñar \\
     hay una tercera cosa.\\
     Adivínala.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Permutacion_de_consecutivos where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  permutaConsecutivos :: [a] -> [a]
\end{descripcion}   
tal que (permutaConsecutivos xs) es la lista obtenida permutando los
elementos consecutivos de xs. Por ejemplo,
\begin{descripcion} 
  permutaConsecutivos [1..8]         ==  [2,1,4,3,6,5,8,7]
  permutaConsecutivos [1..9]         ==  [2,1,4,3,6,5,8,7,9]
  permutaConsecutivos "simplemente"  ==  "ispmelemtne"
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.Array

-- 1ª solución
-- ===========

permutaConsecutivos :: [a] -> [a]
permutaConsecutivos (x:y:zs) = y : x : permutaConsecutivos zs
permutaConsecutivos xs       = xs

-- 2ª solución
-- ===========

permutaConsecutivos2 :: [a] -> [a]
permutaConsecutivos2 xs 
  | even n    = elems (array (1,n) [(i,f i) | i <- [1..n]])
  | otherwise = elems (array (1,n) ((n,v!n) : [(i,f i) | i <- [1..n-1]]))
  where
    n = length xs
    v = listArray (1,n) xs
    f i | even i =    v ! (i - 1)
        | otherwise = v ! (i + 1)
    
-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (permutaConsecutivos [1..(3*10^6)])
--    3000000
--    (2.21 secs, 504,102,648 bytes)
--    λ> length (permutaConsecutivos2 [1..(3*10^6)])
--    3000000
--    (6.18 secs, 1,248,127,688 bytes)
\end{code} 
