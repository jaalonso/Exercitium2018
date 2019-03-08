% Numero_de_descomposiciones_en_sumas_de_cuadrados.lhs
% Número de descomposiciones en sumas de cuadrados.
% José A. Alonso Jiménez
% Sevilla, 1 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Ya habrá cigüeñas al sol, \\
     mirando la tarde roja, \\
     entre Moncayo y Urbión.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numero_de_descomposiciones_en_sumas_de_cuadrados where
\end{code}
\end{comment}

Definir las funciones
\begin{descripcion} 
  nDescomposiciones       :: Int -> Int
  graficaDescomposiciones :: Int -> IO ()
\end{descripcion}   
tales que
\begin{itemize}
\item (nDescomposiciones x) es el número de listas de los cuadrados de
  cuatro números enteros positivos cuya suma es x. Por ejemplo.
\begin{descripcion}   
  nDescomposiciones 4      ==  1
  nDescomposiciones 5      ==  0
  nDescomposiciones 7      ==  4
  nDescomposiciones 10     ==  6
  nDescomposiciones 15     ==  12
  nDescomposiciones 50000  ==  5682
\end{descripcion} 
\item (graficaDescomposiciones n) dibuja la gráfica del número de
  descomposiciones de los n primeros números naturales. Por ejemplo. 
  (graficaDescomposiciones 500) dibuja la Figura \ref{fig:SumasDeCuadrados}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Numero_de_descomposiciones_en_sumas_de_cuadrados.png}
      \caption{(graficaDescomposiciones 500)}
      \label{fig:SumasDeCuadrados}
    \end{figure}
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.Array
import Graphics.Gnuplot.Simple
import Test.QuickCheck

-- 1ª solución
-- ===========

nDescomposiciones :: Int -> Int
nDescomposiciones = length . descomposiciones

-- (descomposiciones x) es la lista de las listas de los cuadrados de
-- cuatro números enteros positivos cuya suma es x. Por  ejemplo. 
--    λ> descomposiciones 4
--    [[1,1,1,1]]
--    λ> descomposiciones 5
--    []
--    λ> descomposiciones 7
--    [[1,1,1,4],[1,1,4,1],[1,4,1,1],[4,1,1,1]]
--    λ> descomposiciones 10
--    [[1,1,4,4],[1,4,1,4],[1,4,4,1],[4,1,1,4],[4,1,4,1],[4,4,1,1]]
--    λ> descomposiciones 15
--    [[1,1,4,9],[1,1,9,4],[1,4,1,9],[1,4,9,1],[1,9,1,4],[1,9,4,1],
--     [4,1,1,9],[4,1,9,1],[4,9,1,1],[9,1,1,4],[9,1,4,1],[9,4,1,1]]
descomposiciones :: Int -> [[Int]]
descomposiciones x = aux x 4
  where 
    aux 0 1 = []
    aux 1 1 = [[1]]
    aux 2 1 = []
    aux 3 1 = []
    aux y 1 | esCuadrado y = [[y]]
            | otherwise    = []
    aux y n = [z^2 : zs | z <- [1..raizEntera y]
                        , zs <- aux (y - z^2) (n-1)]

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Int -> Bool
esCuadrado x = (raizEntera x)^2 == x

-- (raizEntera n) es el mayor entero cuya raíz cuadrada es menor o igual
-- que n. Por ejemplo,
--    raizEntera 15  ==  3
--    raizEntera 16  ==  4
--    raizEntera 17  ==  4
raizEntera :: Int -> Int
raizEntera = floor . sqrt . fromIntegral 

-- 2ª solución
-- =============

nDescomposiciones2 :: Int -> Int
nDescomposiciones2 = length . descomposiciones2

descomposiciones2 :: Int -> [[Int]]
descomposiciones2 x = a ! (x,4)
  where
    a = array ((0,1),(x,4)) [((i,j), f i j) | i <- [0..x], j <- [1..4]]
    f 0 1 = []
    f 1 1 = [[1]]
    f 2 1 = []
    f 3 1 = []
    f i 1 | esCuadrado i = [[i]]
          | otherwise    = []
    f i j = [z^2 : zs | z <- [1..raizEntera i]
                      , zs <- a ! (i - z^2,j-1)]

-- 3ª solución
-- ===========

nDescomposiciones3 :: Int -> Int
nDescomposiciones3 x = aux x 4
  where
    aux 0 1 = 0
    aux 1 1 = 1
    aux 2 1 = 0
    aux 3 1 = 0
    aux y 1 | esCuadrado y = 1
            | otherwise    = 0
    aux y n = sum [aux (y - z^2) (n-1) | z <- [1..raizEntera y]]

-- 4ª solución
-- ===========

nDescomposiciones4 :: Int -> Int
nDescomposiciones4 x = a ! (x,4)
  where
    a = array ((0,1),(x,4)) [((i,j), f i j) | i <- [0..x], j <- [1..4]]
    f 0 1 = 0
    f 1 1 = 1
    f 2 1 = 0
    f 3 1 = 0
    f i 1 | esCuadrado i = 1
          | otherwise    = 0
    f i j = sum [a ! (i- z^2,j-1) | z <- [1..raizEntera i]]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nDescomposiciones :: Positive Int -> Bool
prop_nDescomposiciones (Positive x) =
  all (== nDescomposiciones x) [f x | f <- [ nDescomposiciones2
                                           , nDescomposiciones3
                                           , nDescomposiciones4]]

-- La comprobación es
--    λ> quickCheck prop_nDescomposiciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> nDescomposiciones 20000
--    1068
--    (3.69 secs, 3,307,250,128 bytes)
--    λ> nDescomposiciones2 20000
--    1068
--    (0.72 secs, 678,419,328 bytes)
--    λ> nDescomposiciones3 20000
--    1068
--    (3.94 secs, 3,485,725,552 bytes)
--    λ> nDescomposiciones4 20000
--    1068
--    (0.74 secs, 716,022,456 bytes)
--    
--    λ> nDescomposiciones2 50000
--    5682
--    (2.64 secs, 2,444,206,000 bytes)
--    λ> nDescomposiciones4 50000
--    5682
--    (2.77 secs, 2,582,443,448 bytes)

-- Definición de graficaDescomposiciones
-- =====================================

graficaDescomposiciones :: Int -> IO ()
graficaDescomposiciones n =
  plotList [ Key Nothing
           , PNG ("Numero_de_descomposiciones_en_sumas_de_cuadrados.png")
           ]
           (map nDescomposiciones3 [0..n])
\end{code} 
