% Triangulo_de_Pascal_binario.lhs
% Triángulo de Pascal binario.
% José A. Alonso Jiménez
% Sevilla, 31 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{La envidia de la virtud \\
     hizo a Caín criminal. \\
     ¡Gloria a Caín! Hoy el vicio \\
     es lo que se envidia más.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Triangulo_de_Pascal_binario where
\end{code}
\end{comment}

Los triángulos binarios de Pascal se forman a partir de una lista de
ceros y unos usando las reglas del triángulo de Pascal, donde cada
uno de los números es suma módulo dos de los dos situados en diagonal
por encima suyo. Por ejemplo, los triángulos binarios de Pascal
correspondientes a [1,0,1,1,1] y [1,0,1,1,0] son
\begin{descripcion} 
   1 0 1 1 1   1 0 1 1 0     
    1 1 0 0     1 1 0 1  
     0 1 0       0 1 1   
      1 1         1 0    
       0           1     
\end{descripcion} 
Sus finales, desde el extremo inferior al extremos superior derecho,
son [0,1,0,0,1] y [1,0,1,1,0], respectivamente.

Una lista es Pascal capicúa si es igual a los finales de su triángulo
binario de Pascal. Por ejemplo, [1,0,1,1,0] es Pascal capicúa.  

Definir las funciones
\begin{descripcion} 
  trianguloPascalBinario :: [Int] -> [[Int]]
  pascalCapicuas         :: Int -> [[Int]]
  nPascalCapicuas        :: Int -> Integer
\end{descripcion}   
tales que
\begin{itemize}
\item (trianguloPascalBinario xs) es el triágulo binario de Pascal
  correspondiente a la lista xs. Por ejemplo,
\begin{descripcion}   
  λ> trianguloPascalBinario [1,0,1,1,1]
  [[1,0,1,1,1],[1,1,0,0],[0,1,0],[1,1],[0]]
  λ> trianguloPascalBinario [1,0,1,1,0]
  [[1,0,1,1,0],[1,1,0,1],[0,1,1],[1,0],[1]]
\end{descripcion} 
\item (pascalCapicuas n) es la lista de listas de Pascal capicúas de n
  elementos. Por ejemplo,
\begin{descripcion}   
  λ> pascalCapicuas 2
  [[0,0],[1,0]]
  λ> pascalCapicuas 3
  [[0,0,0],[0,1,0],[1,0,0],[1,1,0]]
  λ> pascalCapicuas 4
  [[0,0,0,0],[0,1,1,0],[1,0,0,0],[1,1,1,0]]
\end{descripcion} 
\item (nPascalCapicuas n) es el número de listas de Pascal capicúas de n
  elementos. Por ejemplo,
\begin{descripcion}   
  λ> nPascalCapicuas 2
  2
  λ> nPascalCapicuas 3
  4
  λ> nPascalCapicuas 4
  4
  λ> nPascalCapicuas 400
  1606938044258990275541962092341162602522202993782792835301376
  λ> length (show (nPascalCapicuas (10^5)))
  15052
  λ> length (show (nPascalCapicuas (10^6)))
  150515
  λ> length (show (nPascalCapicuas (10^7)))
  1505150
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (genericLength, unfoldr)
import Control.Monad (replicateM)

-- Definición de trianguloPascalBinario
-- ====================================

trianguloPascalBinario :: [Int] -> [[Int]]
trianguloPascalBinario xs =
  takeWhile (not . null) (iterate siguiente xs)

-- (siguiente xs) es la línea siguiente a la xs en el triángulo binario
-- de Pascal. Por ejemplo,
--    λ> siguiente [1,0,1,1,1]
--    [1,1,0,0]
--    λ> siguiente it
--    [0,1,0]
--    λ> siguiente it
--    [1,1]
--    λ> siguiente it
--    [0]
--    λ> siguiente it
--    []
--    λ> siguiente it
--    []
siguiente :: [Int] -> [Int]
siguiente xs = [(x + y) `mod` 2 | (x,y) <- zip xs (tail xs)]

-- 2ª definición de trianguloPascalBinario
-- =======================================

trianguloPascalBinario2 :: [Int] -> [[Int]]
trianguloPascalBinario2 = unfoldr f 
  where f [] = Nothing
        f xs = Just (xs, siguiente xs)
 
-- Definición de pascalCapicuas
-- ============================

pascalCapicuas :: Int -> [[Int]]
pascalCapicuas n =
  [xs | xs <- inicios n
      , esPascalCapicua xs]

-- (inicios n) es la lista de longitud n formadas por ceros y unos. Por
-- ejemplo, 
--    λ> inicios 0
--    [[]]
--    λ> inicios 1
--    [[0],[1]]
--    λ> inicios 2
--    [[0,0],[0,1],[1,0],[1,1]]
--    λ> inicios 3
--    [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
inicios :: Int -> [[Int]]
inicios 0 = [[]]
inicios n = map (0:) xss ++ map (1:) xss
  where xss = inicios (n-1)

-- Otra forma de definir inicios es
inicios2 :: Int -> [[Int]]
inicios2 n = sucInicios !! n
  where sucInicios     = iterate siguiente' [[]]
        siguiente' xss = map (0:) xss ++ map (1:) xss

-- Y otra es
inicios3 :: Int -> [[Int]]
inicios3 n = replicateM n [0,1]

-- (esPascalCapicua xs) se verifica si xs es una lista de Pascal
-- capicúa. Por ejemplo, 
--    esPascalCapicua [1,0,1,1,0]  ==  True
--    esPascalCapicua [1,0,1,1,1]  ==  False
esPascalCapicua :: [Int] -> Bool
esPascalCapicua xs =
  xs == finalesTrianguloPascalBinario xs

-- (finalesTrianguloPascalBinario xs) es la inversa de la lista de los
-- finales del triángulo binarios de xs. Por ejemplo,
--    λ> finalesTrianguloPascalBinario [1,0,1,1,1]
--    [0,1,0,0,1]
finalesTrianguloPascalBinario :: [Int] -> [Int]
finalesTrianguloPascalBinario =
  reverse . map last . trianguloPascalBinario

-- 1ª definición de nPascalCapicuas
-- ================================

nPascalCapicuas :: Int -> Integer
nPascalCapicuas =
  genericLength . pascalCapicuas

-- 2ª definición de nPascalCapicuas
-- ================================

nPascalCapicuas2 :: Int -> Integer
nPascalCapicuas2 n =
  2 ^ ((n + 1) `div` 2)  
\end{code} 
