% Divisiones_del_circulo.lhs
% Divisiones del círculo.
% José A. Alonso Jiménez
% Sevilla, 22 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{\dots Y si la vida es corta \\
     y no llega la mar a tu galera, \\
     aguarda sin partir y siempre espera, \\
     que el arte es largo y, además no importa.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Divisiones_del_circulo where
\end{code}
\end{comment}

Dado 4 puntos de un círculo se pueden dibujar 2 cuerdas entre ellos
de forma que no se corten. En efecto, si se enumeran los puntos del 1
al 4 en sentido de las agujas del reloj una forma tiene las cuerdas
\{1--2, 3--4\} y la otra \{1--4, 2--3\}.

Definir la función
\begin{descripcion} 
  numeroFormas :: Integer -> Integer
\end{descripcion}   
tal que (numeroFormas n) es el número de formas que se pueden dibujar
n cuerdas entre 2xn puntos de un círculo sin que se corten. Por
ejemplo,
\begin{descripcion} 
  numeroFormas 1   ==  1
  numeroFormas 2   ==  2
  numeroFormas 4   ==  14
  numeroFormas 75  ==  1221395654430378811828760722007962130791020
  length (show (numeroFormas 1000))  ==  598
\end{descripcion}

\section*{Soluciones}

\begin{code}    
import Data.Array

-- 1ª definición
numeroFormas :: Integer -> Integer
numeroFormas 0 = 0
numeroFormas n = aux (2*n)
  where aux 0 = 1
        aux 2 = 1
        aux i = sum [aux j * aux (i-2-j) | j <- [0,2..i-1]]

-- 2ª definición
numeroFormas2 :: Integer -> Integer
numeroFormas2 0 = 0
numeroFormas2 n = v ! (2*n)
  where v   = array (0,2*n) [(i, f i) | i <- [0..2*n]]
        f 0 = 1
        f 2 = 1
        f i = sum [v ! j * v ! (i-2-j) | j <- [0,2..i-1]]

-- Comparación de eficiencia
-- =========================

--    λ> numeroFormas 15
--    9694845
--    (28.49 secs, 4,293,435,552 bytes)
--    λ> numeroFormas2 15
--    9694845
--    (0.01 secs, 184,552 bytes)
\end{code} 
