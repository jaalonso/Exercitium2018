% Numeros_altamente_compuestos.lhs
% Números altamente compuestos
% José A. Alonso Jiménez
% Sevilla, 16 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Nuestras horas son minutos \\
    cuando esperamos saber, \\
    y siglos cuando sabemos \\
    lo que se puede aprender.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numeros_altamente_compuestos where
\end{code}
\end{comment}

Un número [altamente compuesto](http://bit.ly/2H7Vj61) es un entero
positivo con más divisores que cualquier entero positivo más
pequeño. Por ejemplo,
\begin{itemize}
\item 4 es un número altamente compuesto porque es el menor con 3 divisores,
\item 5 no es altamente compuesto porque tiene menos divisores que 4 y
\item 6 es un número altamente compuesto porque es el menor con 4 divisores,
\end{itemize}

Los primeros números altamente compuestos son
\begin{descripcion} 
  1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, ...
\end{descripcion}
 
Definir las funciones
\begin{descripcion} 
  esAltamenteCompuesto       :: Int -> Bool
  altamenteCompuestos        :: [Int]
  graficaAltamenteCompuestos :: Int -> IO ()
\end{descripcion} 
tales que
\begin{itemize}
\item (esAltamanteCompuesto x) se verifica si x es altamente
compuesto. Por ejemplo,
\begin{descripcion} 
  esAltamenteCompuesto 4      ==  True
  esAltamenteCompuesto 5      ==  False
  esAltamenteCompuesto 6      ==  True
  esAltamenteCompuesto 1260   ==  True
  esAltamenteCompuesto 2520   ==  True
  esAltamenteCompuesto 27720  ==  True
\end{descripcion} 
\item altamente compuestos es la sucesión de los números altamente
  compuestos. Por ejemplo,
\begin{descripcion}   
  λ> take 20 altamenteCompuestos
  [1,2,4,6,12,24,36,48,60,120,180,240,360,720,840,1260,1680,2520,5040,7560]
\end{descripcion} 
\item (graficaAltamenteCompuestos n) dibuja la gráfica de los n primeros
  números altamente compuestos. Por ejemplo, \newline
  (graficaAltamenteCompuestos 25) dibuja
\begin{figure}[h]
  \centering
  \includegraphics[scale=0.5]{imagenes/Numeros_altamente_compuestos.png}
\end{figure}
      
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (group)
import Data.Numbers.Primes (primeFactors)
import Graphics.Gnuplot.Simple

-- 1ª definición de esAltamenteCompuesto
-- =====================================

esAltamenteCompuesto :: Int -> Bool
esAltamenteCompuesto x =
  and [nDivisores x > nDivisores y | y <- [1..x-1]]

-- (nDivisores x) es el número de divisores de x. Por ejemplo,
--    nDivisores 30  ==  8
nDivisores :: Int -> Int
nDivisores x = length (divisores x)

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores x =
  [y | y <- [1..x]
     , x `mod` y == 0]

-- 2ª definición de esAltamenteCompuesto
-- =====================================

esAltamenteCompuesto2 :: Int -> Bool
esAltamenteCompuesto2 x =
  all (nDivisores2 x >) [nDivisores2 y | y <- [1..x-1]]

-- (nDivisores2 x) es el número de divisores de x. Por ejemplo,
--    nDivisores2 30  ==  8
nDivisores2 :: Int -> Int
nDivisores2 = succ . length . divisoresPropios

-- (divisoresPropios x) es la lista de los divisores de x menores que
-- x. Por ejemplo, 
--    divisoresPropios 30  ==  [1,2,3,5,6,10,15]
divisoresPropios :: Int -> [Int]
divisoresPropios x =
  [y | y <- [1..x `div` 2]
     , x `mod` y == 0]

-- 3ª definición de esAltamenteCompuesto
-- =====================================

esAltamenteCompuesto3 :: Int -> Bool
esAltamenteCompuesto3 x =
  all (nDivisores3 x >) [nDivisores3 y | y <- [1..x-1]]

-- (nDivisores3 x) es el número de divisores de x. Por ejemplo,
--    nDivisores3 30  ==  8
nDivisores3 :: Int -> Int
nDivisores3 x =
  product [1 + length xs | xs <- group (primeFactors x)]

-- 4ª definición de esAltamenteCompuesto
-- =====================================

esAltamenteCompuesto4 :: Int -> Bool
esAltamenteCompuesto4 x =
  x `pertenece` altamenteCompuestos2

-- 1ª definición de altamenteCompuestos 
-- ====================================

altamenteCompuestos :: [Int]
altamenteCompuestos =
  filter esAltamenteCompuesto4 [1..]

-- 2ª definición de altamenteCompuestos 
-- ====================================

altamenteCompuestos2 :: [Int]
altamenteCompuestos2 =
  1 : [y | ((_,n),(y,m)) <- zip sucMaxDivisores (tail sucMaxDivisores)
         , m > n]

-- sucMaxDivisores es la sucesión formada por los números enteros
-- positivos y el máximo número de divisores hasta cada número. Por
-- ejemplo,
--    λ> take 12 sucMaxDivisores
--    [(1,1),(2,2),(3,2),(4,3),(5,3),(6,4),(7,4),(8,4),(9,4),(10,4),(11,4),(12,6)]
sucMaxDivisores :: [(Int,Int)]
sucMaxDivisores =
  zip [1..] (scanl1 max (map nDivisores3 [1..]))

pertenece :: Int -> [Int] -> Bool
pertenece x ys =
  x == head (dropWhile (<x) ys)

-- Comparación de eficiencia de esAltamenteCompuesto
-- =================================================

--    λ> esAltamenteCompuesto 1260
--    True
--    (2.99 secs, 499,820,296 bytes)
--    λ> esAltamenteCompuesto2 1260
--    True
--    (0.51 secs, 83,902,744 bytes)
--    λ> esAltamenteCompuesto3 1260
--    True
--    (0.04 secs, 15,294,192 bytes)
--    λ> esAltamenteCompuesto4 1260
--    True
--    (0.04 secs, 15,594,392 bytes)
--    
--    λ> esAltamenteCompuesto2 2520
--    True
--    (2.10 secs, 332,940,168 bytes)
--    λ> esAltamenteCompuesto3 2520
--    True
--    (0.09 secs, 37,896,168 bytes)
--    λ> esAltamenteCompuesto4 2520
--    True
--    (0.06 secs, 23,087,456 bytes)
--
--    λ> esAltamenteCompuesto3 27720
--    True
--    (1.32 secs, 841,010,624 bytes)
--    λ> esAltamenteCompuesto4 27720
--    True
--    (1.33 secs, 810,870,384 bytes)

-- Comparación de eficiencia de altamenteCompuestos
-- ================================================

--    λ> altamenteCompuestos !! 25
--    45360
--    (2.84 secs, 1,612,045,976 bytes)
--    λ> altamenteCompuestos2 !! 25
--    45360
--    (0.01 secs, 102,176 bytes)

-- Definición de graficaAltamenteCompuestos
-- ========================================

graficaAltamenteCompuestos :: Int -> IO ()
graficaAltamenteCompuestos n =
  plotList [ Key Nothing
           , PNG ("Numeros_altamente_compuestos.png")
           ]
           (take n altamenteCompuestos2)
\end{code} 
