% Numero_de_particiones_de_un_conjunto.lhs
% Número de particiones de un conjunto.
% José A. Alonso Jiménez
% Sevilla, 27 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Yo he visto garras fieras en las pulidas manos; \\
     conozco grajos mélicos y líricos marranos \dots \\
     El más truhán se lleva la mano al corazón, \\
     y el bruto más espeso se carga de razón.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numero_de_particiones_de_un_conjunto where
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
  nParticiones :: [a] -> Integer
\end{descripcion} 
tal que (nParticiones xs) es el número de particiones de xs. Por
ejemplo,
\begin{descripcion} 
  nParticiones [1,2]                     ==  2
  nParticiones [1,2,3]                   ==  5
  nParticiones "abcd"                    ==  15
  length (show (nParticiones [1..500]))  ==  844
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List  ( genericLength
                  )
import Data.Array ( Array
                  , (!)
                  , array
                  )

-- 1ª definición
-- =============

nParticiones :: [a] -> Integer
nParticiones xs =
  genericLength (particiones xs)

-- (particiones xs) es el conjunto de las particiones de xs. Por
-- ejemplo, 
--    λ> particiones [1,2]
--    [[[1,2]],[[1],[2]]]
--    λ> particiones [1,2,3]
--    [[[1,2,3]],[[1],[2,3]],[[1,2],[3]],[[2],[1,3]],[[1],[2],[3]]]
particiones :: [a] -> [[[a]]]
particiones [] = [[]]
particiones xs =
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

-- (inserta x yss) es la lista obtenida insertando x en cada uno de los
-- elementos de yss. Por ejemplo, 
--    λ> inserta 1 [[2,3],[4],[5,6,7]]
--    [[[1,2,3],[4],[5,6,7]],[[2,3],[1,4],[5,6,7]],[[2,3],[4],[1,5,6,7]]]
inserta :: a -> [[a]] -> [[[a]]]
inserta _ []       = []
inserta x (ys:yss) = ((x:ys):yss) : [ys : zs | zs <- inserta x yss] 

-- 2ª definición
-- =============

nParticiones2 :: [a] -> Integer
nParticiones2 xs = sum [nParticionesFijas n k | k <- [0..n]]
  where n = genericLength xs

-- nPparticionesFijas n k) es el número de las particiones de un
-- conjunto con n elementos en k subconjuntos. Por ejemplo,
--    nParticionesFijas 3 0  ==  0
--    nParticionesFijas 3 1  ==  1
--    nParticionesFijas 3 2  ==  3
--    nParticionesFijas 3 3  ==  1
--    nParticionesFijas 3 4  ==  0
nParticionesFijas :: Integer -> Integer -> Integer
nParticionesFijas 0 0 = 1
nParticionesFijas 0 _ = 0
nParticionesFijas _ 1 = 1
nParticionesFijas n k =
  nParticionesFijas (n-1) (k-1) + k * nParticionesFijas (n-1) k

-- 3ª definición
-- =============

nParticiones3 :: [a] -> Integer
nParticiones3 xs = sum [a ! (n,k) | k <- [0..n]]
  where n = genericLength xs
        a = matrizNParticiones n

-- (matrizNParticiones n) es la matriz de dimensión ((0,0),(n,n)) que en
-- la posición (i,j) tiene el número de particiones de un conjunto de i
-- elementos en j subconjuntos. Por ejemplo,
--    λ> matrizNParticiones 3
--    array ((0,0),(3,3))
--          [((0,0),0),((0,1),0),((0,2),0),((0,3),0),
--           ((1,0),0),((1,1),1),((1,2),0),((1,3),0),
--           ((2,0),0),((2,1),1),((2,2),1),((2,3),0),
--           ((3,0),0),((3,1),1),((3,2),3),((3,3),1)]
--    λ> matrizNParticiones 4
--    array ((0,0),(4,4))
--          [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((0,4),0),
--           ((1,0),0),((1,1),1),((1,2),0),((1,3),0),((1,4),0),
--           ((2,0),0),((2,1),1),((2,2),1),((2,3),0),((2,4),0),
--           ((3,0),0),((3,1),1),((3,2),3),((3,3),1),((3,4),0),
--           ((4,0),0),((4,1),1),((4,2),7),((4,3),6),((4,4),1)]
matrizNParticiones :: Integer -> Array (Integer,Integer) Integer
matrizNParticiones n = a 
  where
    a = array ((0,0),(n,n)) [((i,j), f i j) | i <- [0..n], j <- [0..n]]
    f 0 0 = 1
    f 0 _ = 0
    f _ 0 = 0
    f _ 1 = 1
    f i j = a ! (i-1,j-1) + j * a ! (i-1,j)

-- 4ª definición
-- =============

nParticiones4 :: [a] -> Integer
nParticiones4 xs = sum [a ! (n,k) | k <- [0..n]]
  where
    n = genericLength xs
    a = array ((0,0),(n,n)) [((i,j), f i j) | i <- [0..n], j <- [0..n]]
    f 0 0 = 1
    f 0 _ = 0
    f _ 0 = 0
    f _ 1 = 1
    f i j = a ! (i-1,j-1) + j * a ! (i-1,j)

-- Comparación de eficiencia
-- =========================

--    λ> nParticiones [1..11]
--    678570
--    (3.77 secs, 705,537,480 bytes)
--    λ> nParticiones2 [1..11]
--    678570
--    (0.07 secs, 6,656,584 bytes)
--    λ> nParticiones3 [1..11]
--    678570
--    (0.01 secs, 262,176 bytes)
--    λ> nParticiones4 [1..11]
--    678570
--    (0.01 secs, 262,264 bytes)
--    
--    λ> nParticiones2 [1..16]
--    10480142147
--    (2.24 secs, 289,774,408 bytes)
--    λ> nParticiones3 [1..16]
--    10480142147
--    (0.01 secs, 437,688 bytes)
--    λ> nParticiones4 [1..16]
--    10480142147
--    (0.01 secs, 437,688 bytes)
--    
--    λ> length (show (nParticiones3 [1..500]))
--    844
--    (2.23 secs, 357,169,528 bytes)
--    λ> length (show (nParticiones4 [1..500]))
--    844
--    (2.20 secs, 357,172,680 bytes)
\end{code} 
