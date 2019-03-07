% Descomposiciones_en_sumas_de_cuadrados.lhs
% Descomposiciones en sumas de cuadrados.
% José A. Alonso Jiménez
% Sevilla, 28 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{No extrañéis, dulces amigos, \\
     que esté mi frente arrugada; \\
     yo vivo en paz con los hombres \\
     y en guerra con mis entrañas.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Descomposiciones_en_sumas_de_cuadrados where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  descomposiciones :: Int -> [[Int]]
\end{descripcion} 
tal que (descomposiciones x) es la lista de las listas de los
cuadrados de cuatro números enteros positivos cuya suma es x. Por
ejemplo.
\begin{descripcion} 
  λ> descomposiciones 4
  [[1,1,1,1]]
  λ> descomposiciones 5
  []
  λ> descomposiciones 7
  [[1,1,1,4],[1,1,4,1],[1,4,1,1],[4,1,1,1]]
  λ> descomposiciones 10
  [[1,1,4,4],[1,4,1,4],[1,4,4,1],[4,1,1,4],[4,1,4,1],[4,4,1,1]]
  λ> descomposiciones 15
  [[1,1,4,9],[1,1,9,4],[1,4,1,9],[1,4,9,1],[1,9,1,4],[1,9,4,1],
   [4,1,1,9],[4,1,9,1],[4,9,1,1],[9,1,1,4],[9,1,4,1],[9,4,1,1]]
  λ> length (descomposiciones 50000)
  5682
\end{descripcion}

\section*{Soluciones}

\begin{code}    
import Data.Array
import Test.QuickCheck

-- 1ª definición
-- =============

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

-- 2ª definición
-- =============

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

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_descomposiciones :: Positive Int -> Bool
prop_descomposiciones (Positive x) =
  descomposiciones x == descomposiciones2 x

-- La comprobación es
--    λ> quickCheck prop_descomposiciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (descomposiciones (2*10^4))
--    1068
--    (3.70 secs, 3,307,251,704 bytes)
--    λ> length (descomposiciones2 (2*10^4))
--    1068
--    (0.72 secs, 678,416,144 bytes)
\end{code} 
