% Reconocimiento_de_conmutatividad.lhs
% Reconocimiento de conmutatividad.
% José A. Alonso Jiménez
% Sevilla, 27 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Reconocimiento_de_conmutatividad where
\end{code}
\end{comment}

Para representar las operaciones binarias en un conjunto finito A con
n elementos se pueden numerar sus elementos desde el 0 al n-1.
Entonces cada operación binaria en A se puede ver como una lista 
de listas xss tal que el valor de aplicar la operación a los elementos
i y j es el j-ésimo elemento del i-ésimo elemento de xss. Por
ejemplo, si A = {0,1,2} entonces las tabla de la suma y de la resta
módulo 3 en A son
\begin{descripcion} 
   0 1 2    0 2 1
   1 2 0    1 0 2
   2 0 1    2 1 0
   Suma     Resta
\end{descripcion}
 
Definir la función
\begin{descripcion} 
   conmutativa :: [[Int]] -> Bool
\end{descripcion}   
tal que (conmutativa xss) se verifica si la operación cuya tabla es
xss es conmutativa. Por ejemplo,
\begin{descripcion} 
   conmutativa [[0,1,2],[1,0,1],[2,1,0]]  ==  True
   conmutativa [[0,1,2],[1,0,0],[2,1,0]]  ==  False
   conmutativa [[i+j `mod` 2000 | j <- [0..1999]] | i <- [0..1999]] == True
   conmutativa [[i-j `mod` 2000 | j <- [0..1999]] | i <- [0..1999]] == False
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.List (transpose)
import Test.QuickCheck

-- 1ª solución
-- ===========

conmutativa :: [[Int]] -> Bool
conmutativa xss =
  and [producto i j == producto j i | i <- [0..n-1], j <- [0..n-1]]
  where producto i j = (xss !! i) !! j
        n            = length xss

-- 2ª solución
-- ===========

conmutativa2 :: [[Int]] -> Bool
conmutativa2 []         = True
conmutativa2 t@(xs:xss) = xs == map head t
                          && conmutativa2 (map tail xss)

-- 3ª solución
-- ===========

conmutativa3 :: [[Int]] -> Bool
conmutativa3 xss = xss == transpose xss

-- 4ª solución
-- ===========

conmutativa4 :: [[Int]] -> Bool
conmutativa4 = (==) <*> transpose 

-- Equivalencia de las definiciones
-- ================================

-- Para comprobar la equivalencia se define el tipo de tabla de
-- operciones binarias:
newtype Tabla = T [[Int]]
  deriving Show

-- genTabla es un generador de tablas de operciones binaria. Por ejemplo,
--    λ> sample genTabla
--    T [[2,0,0],[1,2,1],[1,0,2]]
--    T [[0,3,0,1],[0,1,2,1],[0,2,1,2],[3,0,0,2]]
--    T [[2,0,1],[1,0,0],[2,1,2]]
--    T [[1,0],[0,1]]
--    T [[1,1],[0,1]]
--    T [[1,1,2],[1,0,1],[2,1,0]]
--    T [[4,4,3,0,2],[2,2,0,1,2],[4,0,1,0,0],[0,4,4,3,3],[3,0,4,2,1]]
--    T [[3,4,1,4,1],[2,4,4,0,4],[1,2,1,4,3],[3,1,4,4,2],[4,1,3,2,3]]
--    T [[2,0,1],[2,1,0],[0,2,2]]
--    T [[3,2,0,3],[2,1,1,1],[0,2,1,0],[3,3,2,3]]
--    T [[2,0,2,0],[0,0,3,1],[1,2,3,2],[3,3,0,2]]
genTabla :: Gen Tabla
genTabla = do
  n  <- choose (2,20)
  xs <- vectorOf (n^2) (elements [0..n-1])
  return (T (separa n xs))

-- (separa n xs) es la lista obtenidaseparando los elementos de xs en
-- grupos de n elementos. Por ejemplo,
--    separa 3 [1..9]  ==  [[1,2,3],[4,5,6],[7,8,9]]
separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xs = take n xs : separa n (drop n xs)

-- Generación arbitraria de tablas
instance Arbitrary Tabla where
  arbitrary = genTabla

-- La propiedad es
prop_conmutativa :: Tabla -> Bool
prop_conmutativa (T xss) =
  conmutativa xss  == conmutativa2 xss &&
  conmutativa2 xss == conmutativa3 xss &&
  conmutativa2 xss == conmutativa4 xss

-- La comprobación es
--    λ> quickCheck prop_conmutativa
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- Para las comparaciones se usará la función tablaSuma tal que
-- (tablaSuma n) es la tabla de la suma módulo n en [0..n-1]. Por
-- ejemplo, 
--    tablaSuma 3  ==  [[0,1,2],[1,2,3],[2,3,4]]
tablaSuma ::  Int -> [[Int]]
tablaSuma n =
  [[i + j `mod` n | j <- [0..n-1]] | i <- [0..n-1]]

-- La comparación es
--    λ> conmutativa (tablaSuma 400)
--    True
--    (1.92 secs, 147,608,696 bytes)
--    λ> conmutativa2 (tablaSuma 400)
--    True
--    (0.14 secs, 63,101,112 bytes)
--    λ> conmutativa3 (tablaSuma 400)
--    True
--    (0.10 secs, 64,302,608 bytes)
--    λ> conmutativa4 (tablaSuma 400)
--    True
--    (0.10 secs, 61,738,928 bytes)
--    
--    λ> conmutativa2 (tablaSuma 2000)
--    True
--    (1.81 secs, 1,569,390,480 bytes)
--    λ> conmutativa3 (tablaSuma 2000)
--    True
--    (3.07 secs, 1,601,006,840 bytes)
--    λ> conmutativa4 (tablaSuma 2000)
--    True
--    (3.14 secs, 1,536,971,288 bytes)
\end{code} 
