% Numero_como_suma_de_sus_digitos.lhs
% Número como suma de sus dígitos.
% José A. Alonso Jiménez
% Sevilla, 6 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Caminante, son tus huellas \\
     el camino, y nada más; \\
     caminante no hay camino, \\
     se hace camino al andar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numero_como_suma_de_sus_digitos where
\end{code}
\end{comment}

El número 23 se puede escribir de 4 formas como suma de sus dígitos
\begin{descripcion} 
   2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 3
   2 + 2 + 2 + 2 + 2 + 2 + 2 + 3 + 3 + 3
   2 + 2 + 2 + 2 + 3 + 3 + 3 + 3 + 3
   2 + 3 + 3 + 3 + 3 + 3 + 3 + 3
\end{descripcion} 
La de menor número de sumando es la última, que tiene 8 sumandos.

Definir las funciones
\begin{descripcion} 
   minimoSumandosDigitos        :: Integer -> Integer
   graficaMinimoSumandosDigitos :: Integer -> IO ()
\end{descripcion} 
tales que
\begin{itemize}
\item (minimoSumandosDigitos n) es el menor número de dígitos de n cuya
  suma es n. Por ejemplo,
\begin{descripcion}   
  minimoSumandosDigitos 23    ==  8
  minimoSumandosDigitos 232   ==  78
  minimoSumandosDigitos 2323  ==  775
  map minimoSumandosDigitos [10..20] == [10,11,6,5,5,3,6,5,4,3,10]
\end{descripcion} 
\item (graficaMinimoSumandosDigitos n) dibuja la gráfica de
  (minimoSumandosDigitos k) par los k primeros números naturales. Por
  ejemplo, (graficaMinimoSumandosDigitos 300) dibuja

  la Figura \ref{fig:SumaDigitos}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Numero_como_suma_de_sus_digitos.png}
      \caption{(graficaMinimoSumandosDigitos 300)}
      \label{fig:SumaDigitos}
    \end{figure}

\end{itemize}

\section*{Soluciones}

\begin{code}   
import Test.QuickCheck
import Graphics.Gnuplot.Simple
import Data.List (nub, genericLength, sort)

minimoSumandosDigitos :: Integer -> Integer
minimoSumandosDigitos n =
  minimoSumandos (digitos n) n

-- (digitos n) es el conjunto de los dígitos no nulos de n. Por ejemplo,
--    digitos 2032  ==  [2,3]
digitos :: Integer -> [Integer]
digitos n =
  nub [read [c] | c <- show n, c /= '0']

-- (minimoSumandos xs n) es el menor número de elementos de la lista de
-- enteros positivos xs (con posibles repeticiones) cuya suma es n. Por
-- ejemplo, 
--    minimoSumandos [7,2,4] 11  ==  2
minimoSumandos :: [Integer] -> Integer -> Integer
minimoSumandos xs n =
  minimum (map genericLength (sumas xs n))

-- (sumas xs n) es la lista de elementos de la lista de enteros
-- positivos xs (con posibles repeticiones) cuya suma es n. Por ejemplo,  
--    sumas [7,2,4] 11  ==  [[7,2,2],[7,4]]
sumas :: [Integer] -> Integer -> [[Integer]]
sumas [] 0 = [[]]
sumas [] _ = []
sumas (x:xs) n
  | x <= n    = map (x:) (sumas (x:xs) (n-x)) ++ sumas xs n
  | otherwise = sumas xs n

-- 2ª solución
-- ===========

minimoSumandosDigitos2 :: Integer -> Integer
minimoSumandosDigitos2 n =
  minimoSumandos (digitos2 n) n

-- (digitos n) es el conjunto de los dígitos no nulos de n. Por ejemplo,
--    digitos 2032  ==  [2,3]
digitos2 :: Integer -> [Integer]
digitos2 n =
  reverse (sort (nub [read [c] | c <- show n, c /= '0']))

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_minimoSumandosDigitos :: Positive Integer -> Bool
prop_minimoSumandosDigitos (Positive n) =
  minimoSumandosDigitos n == minimoSumandosDigitos2 n

-- La comprobación es
--    λ> quickCheck prop_minimoSumandosDigitos
--    +++ OK, passed 100 tests.

-- Definición de graficaMinimoSumandosDigitos
-- ==========================================

graficaMinimoSumandosDigitos :: Integer -> IO ()
graficaMinimoSumandosDigitos n =
  plotList [ Key Nothing
           -- , PNG "Numero_como_suma_de_sus_digitos.png"
           ]
           [minimoSumandosDigitos k | k <- [0..n-1]]
\end{code} 
