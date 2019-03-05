% Particiones_de_un_conjunto.lhs
% Particiones de un conjunto.
% José A. Alonso Jiménez
% Sevilla, 26 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{A quien nos justifica nuestra desconfianza \\
     llamamos enemigo, ladrón de una esperanza. \\
     Jamás perdona el necio si ve la nuez vacía \\
     que dio a cascar al diente de la sabiduría.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Particiones_de_un_conjunto where
\end{code}
\end{comment}

Una partición de un conjunto A es un conjunto de subconjuntos no
vacíos de A, disjuntos dos a dos y cuya unión es A. Por ejemplo, el
conjunto \{1, 2, 3\} tiene exactamente 5 particiones:
\begin{descripcion} 
  {{1}, {2}, {3}}
  {{1,2}, {3}}
  {{1,3}, {2}}
  {{1}, {2,3}}
  {{1,2,3}}
\end{descripcion}

Definir la función
\begin{descripcion} 
  particiones :: [a] -> [[[a]]]
\end{descripcion}   
tal que (particiones xs) es el conjunto de las particiones de xs. Por
ejemplo,
\begin{descripcion} 
  λ> particiones [1,2]
  [[[1,2]],[[1],[2]]]
  λ> particiones [1,2,3]
  [[[1,2,3]],[[1],[2,3]],[[1,2],[3]],[[2],[1,3]],[[1],[2],[3]]]
  λ> particiones "abcd"
  [["abcd"],["a","bcd"],["ab","cd"],["b","acd"],["abc","d"],["bc","ad"],
   ["ac","bd"],["c","abd"],["a","b","cd"],["a","bc","d"],["a","c","bd"],
   ["ab","c","d"],["b","ac","d"],["b","c","ad"],["a","b","c","d"]]
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.List (sort)
import Data.Array
import Test.QuickCheck

-- 1ª solución
-- ===========

particiones :: [a] -> [[[a]]]
particiones [] = [[]]
particiones (x:xs) =
  concat [([x] : yss) : inserta x yss | yss <- ysss]
  where ysss = particiones xs

-- (inserta x yss) es la lista obtenida insertando x en cada uno de los
-- elementos de yss. Por ejemplo, 
--    λ> inserta 1 [[2,3],[4],[5,6,7]]
--    [[[1,2,3],[4],[5,6,7]],[[2,3],[1,4],[5,6,7]],[[2,3],[4],[1,5,6,7]]]
inserta :: a -> [[a]] -> [[[a]]]
inserta _ []       = []
inserta x (ys:yss) = ((x:ys):yss) : [ys : zs | zs <- inserta x yss] 

-- 2ª solución
-- ===========

particiones2 :: [a] -> [[[a]]]
particiones2 [] = [[]]
particiones2 xs =
  concat [particionesFijas xs k | k <- [0..length xs]]

-- (particionesFijas xs k) es el conjunto de las particiones de xs en k
-- subconjuntos. Por ejemplo,
--    particionesFijas [1,2,3] 0  ==  []
--    particionesFijas [1,2,3] 1  ==  [[[1,2,3]]]
--    particionesFijas [1,2,3] 2  ==  [[[1],[2,3]],[[1,2],[3]],[[2],[1,3]]]
--    particionesFijas [1,2,3] 3  ==  [[[1],[2],[3]]]
--    particionesFijas [1,2,3] 4  ==  []
particionesFijas :: [a] -> Int -> [[[a]]]
particionesFijas [] _ = []
particionesFijas xs 1 = [[xs]]
particionesFijas (x:xs) k =
   [[x]:ys | ys <- particionesFijas xs (k-1)] ++
   concat [inserta x ys | ys <- particionesFijas xs k]

-- 3ª solución
-- ===========

particiones3 :: [a] -> [[[a]]]
particiones3 xs = concat [a ! (n,k) | k <- [0..n]]
  where a = matrizParticiones xs
        n = length xs

-- (matrizParticiones xs) es la matriz de dimensión ((0,0),(n,n)) que en
-- la posición (i,j) tiene el conjunto de particiones de los i primeros
-- elementoa de xs en j subconjuntos. Por ejemplo,
--   λ> elems (matrizParticiones [1,2,3])
--   [[[]],[],         [],                                   [],
--    [],  [[[1]]],    [],                                   [],
--    [],  [[[1,2]]],  [[[2],[1]]],                          [],
--    [],  [[[1,2,3]]],[[[3],[1,2]],[[3,2],[1]],[[2],[3,1]]],[[[3],[2],[1]]]]
matrizParticiones :: [a] -> Array (Int,Int) [[[a]]]
matrizParticiones xs = a 
  where
    n = length xs
    v = listArray (1,n) xs
    a = array ((0,0),(n,n)) [((i,j), f i j) | i <- [0..n], j <- [0..n]]
    f 0 0 = [[]]
    f 0 _ = []
    f _ 0 = []
    f i 1 = [[[v!k | k <- [1..i]]]]
    f i j = [[v!i] : ys | ys <- a!(i-1,j-1)] ++
            concat [inserta (v!i) ys | ys <- a!(i-1,j)]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_Particiones :: [Int] -> Bool
prop_Particiones xs =
  all (== (ordenada . particiones) xs)
      [(ordenada . f )xs | f <- [ particiones2
                                , particiones3]]
        
ordenada :: Ord a => [[[a]]] -> [[[a]]]
ordenada xsss =
  sort [sort (map sort xss) | xss <- xsss]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_Particiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> length (particiones [1..12])
--    4213597
--    (2.74 secs, 2,903,492,120 bytes)
--    λ> length (particiones2 [1..12])
--    4213597
--    (4.63 secs, 3,878,003,920 bytes)
--    λ> length (particiones3 [1..12])
--    4213597
--    (6.21 secs, 3,199,076,464 bytes)
\end{code} 
