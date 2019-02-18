% Particiones_de_enteros_positivos.lhs
% Particiones de enteros positivos.
% José A. Alonso Jiménez
% Sevilla, 11 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Fe empirista. Ni somos ni seremos. \\
     Todo nuestro vivir es emprestado. \\
     Nada trajimos, nada llevaremos.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Particiones_de_enteros_positivos where
\end{code}
\end{comment}

Una \href{http://bit.ly/1KtLkNZ}{partición} de un entero positivo n es una
manera de escribir n como una suma de enteros positivos. Dos sumas
que sólo difieren en el orden de sus sumandos se consideran la misma
partición. Por ejemplo, 4 tiene cinco particiones: 4, 3+1, 2+2, 2+1+1
y 1+1+1+1.

Definir la función
\begin{descripcion} 
  particiones :: Int -> [[Int]]
\end{descripcion} 
tal que (particiones n) es la lista de las particiones del número
n. Por ejemplo,
\begin{descripcion} 
  particiones 4  ==  [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
  particiones 5  ==  [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]
  length (particiones 50)  ==  204226
\end{descripcion}

\section*{Soluciones}

\begin{code} 
-- 1ª solución
particiones :: Int -> [[Int]]
particiones 0 = [[]]
particiones n = [x:y | x <- [n,n-1..1], 
                       y <- particiones (n-x), 
                       [x] >= take 1 y]
 
-- 2ª solución
particiones2 :: Int -> [[Int]]
particiones2 n = aux !! n where
  aux = [] : map particiones' [1..]  
  particiones' n' = [n'] : [x:p | x <- [n',n'-1..1], 
                                  p <- aux !! (n'-x), 
                                  x >= head p]

-- Comparación de eficiencia                                        --
-- =========================

-- La comparación es
--    ghci> length (particiones 20)
--    627
--    (4.93 secs, 875288184 bytes)
--     
--    ghci> length (particiones2 20)
--    627
--    (0.02 secs, 2091056 bytes)
\end{code} 
