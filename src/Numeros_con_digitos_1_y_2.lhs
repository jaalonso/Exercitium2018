% Numeros_con_digitos_1_y_2.lhs
% Números con dígitos 1 y 2.
% José A. Alonso Jiménez
% Sevilla, 28 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Para qué llamar caminos \\
     a los surcos del azar? \dots \\
     Todo el que camina anda, \\
     como Jesús, sobre el mar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numeros_con_digitos_1_y_2 where
\end{code}
\end{comment}

Definir las funciones
\begin{descripcion} 
  numerosCon1y2               :: Int -> [Int]
  restosNumerosCon1y2         :: Int -> [Int]
  grafica_restosNumerosCon1y2 :: Int -> IO ()
\end{descripcion} 
tales que
\begin{itemize}
\item (numerosCon1y2 n) es la lista ordenada de números de n dígitos que
  se pueden formar con los dígitos 1 y 2. Por ejemplo,
  \begin{descripcion}   
    numerosCon1y2 2  ==  [11,12,21,22] 
    numerosCon1y2 3  ==  [111,112,121,122,211,212,221,222]
  \end{descripcion} 
\item (restosNumerosCon1y2 n) es la lista de los restos de dividir los
  elementos de (restosNumerosCon1y2 n) entre \(2^n\). Por ejemplo,
  \begin{descripcion} 
    restosNumerosCon1y2 2 == [3,0,1,2]
    restosNumerosCon1y2 3 == [7,0,1,2,3,4,5,6]
    restosNumerosCon1y2 4 == [7,8,1,2,11,12,5,6,15,0,9,10,3,4,13,14]
  \end{descripcion} 
\item \verb|(grafica_restosNumerosCon1y2 n)| dibuja la gráfica de los restos de
  dividir los elementos de (restosNumerosCon1y2 n) entre \(2^n\). Por
  ejemplo,
  \begin{itemize}
  \item \verb|(grafica_restosNumerosCon1y2 3)| dibuja la Figura
  \ref{fig:NumerosConDigitos1} 
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Numeros_con_digitos_1_y_2_3.png}
      \caption{(graficaPrimosEnPi 10 10000)}
      \label{fig:NumerosConDigitos1}
    \end{figure}
  \item \verb|(grafica_restosNumerosCon1y2 4)| dibuja la Figura
  \ref{fig:NumerosConDigitos2} 
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Numeros_con_digitos_1_y_2_4.png}
      \caption{(graficaPrimosEnPi 10 10000)}
      \label{fig:NumerosConDigitos2}
    \end{figure}
  \item \verb|(grafica_restosNumerosCon1y2 5)| dibuja la Figura
  \ref{fig:NumerosConDigitos3} 
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Numeros_con_digitos_1_y_2_5.png}
      \caption{(graficaPrimosEnPi 10 10000)}
      \label{fig:NumerosConDigitos3}
    \end{figure}
  \end{itemize}
  \textbf{Nota}: En la definición usar la función
  \href{http://bit.ly/2HzYwvA}{plotListStyle} y como segundo argumento
  usar
  \begin{descripcion} 
    (defaultStyle {plotType = LinesPoints,
                   lineSpec = CustomStyle [PointType 7]})
  \end{descripcion}  
\end{itemize}

Comprobar con QuickCheck que todos los elementos de
(restosNumerosCon1y2 n) son distintos.

\section*{Soluciones}

\begin{code} 
import Data.List (nub)
import Test.QuickCheck
import Graphics.Gnuplot.Simple
import Control.Monad (replicateM)

-- 1ª definición de numerosCon1y2
-- ==============================

numerosCon1y2 :: Int -> [Int]
numerosCon1y2 = map digitosAnumero . digitos

-- (dígitos n) es la lista ordenada de de listas de n elementos que
-- se pueden formar con los dígitos 1 y 2. Por ejemplo,
--    λ> digitos 2
--    [[1,1],[1,2],[2,1],[2,2]]
--    λ> digitos 3
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
digitos :: Int -> [[Int]]
digitos 0 = [[]]
digitos n = map (1:) xss ++ map (2:) xss
  where xss = digitos (n-1)

-- (digitosAnumero ds) es el número cuyos dígitos son ds. Por ejemplo,
--    digitosAnumero [2,0,1,9]  ==  2019
digitosAnumero :: [Int] -> Int
digitosAnumero = read . concatMap show

-- 2ª definición de numerosCon1y2
-- ==============================

numerosCon1y22 :: Int -> [Int]
numerosCon1y22 = map digitosAnumero . digitos2

-- (dígitos2 n) es la lista ordenada de las listas de n elementos que
-- se pueden formar con los dígitos 1 y 2. Por ejemplo,
--    λ> digitos2 2
--    [[1,1],[1,2],[2,1],[2,2]]
--    λ> digitos2 3
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
digitos2 :: Int -> [[Int]]
digitos2 n = sucDigitos !! n

-- sucDigitos es la lista ordenada de las listas que se pueden formar
-- con los dígitos 1 y 2. Por ejemplo, 
--    λ> take 4 sucDigitos
--    [[[]],
--     [[1],[2]],
--     [[1,1],[1,2],[2,1],[2,2]],
--     [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]]
sucDigitos :: [[[Int]]]
sucDigitos = iterate siguiente [[]]
  where siguiente xss = map (1:) xss ++ map (2:) xss

-- 3ª definición de numerosCon1y2
-- ==============================

numerosCon1y23 :: Int -> [Int]
numerosCon1y23 = map digitosAnumero . digitos2

-- (digitos3 n) es la lista ordenada de las listas de n elementos que
-- se pueden formar con los dígitos 1 y 2. Por ejemplo,
--    λ> digitos3 2
--    [[1,1],[1,2],[2,1],[2,2]]
--    λ> digitos3 3
--    [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]]
digitos3 :: Int -> [[Int]]
digitos3 n = replicateM n [1,2]

-- Definición de restosNumerosCon1y2
-- =================================

restosNumerosCon1y2 :: Int -> [Int]
restosNumerosCon1y2 n =
  [x `mod` m | x <- numerosCon1y2 n]
  where m = 2^n

-- Definición de grafica_restosNumerosCon1y2
-- =========================================

grafica_restosNumerosCon1y2 :: Int -> IO ()
grafica_restosNumerosCon1y2 n =
  plotListStyle
    [ Key Nothing
    -- , PNG ("Numeros_con_digitos_1_y_2_" ++ show n ++ ".png")
    ]
    (defaultStyle {plotType = LinesPoints,
                   lineSpec = CustomStyle [PointType 7]})
    (restosNumerosCon1y2 n)

-- Propiedad de restosNumerosCon1y2
-- ================================

-- La propiedad
prop_restosNumerosCon1y2 :: Positive Int -> Bool
prop_restosNumerosCon1y2 (Positive n) =
  todosDistintos (restosNumerosCon1y2 n)
  where todosDistintos xs = xs == nub xs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=12}) prop_restosNumerosCon1y2
--    +++ OK, passed 100 tests.
\end{code} 
