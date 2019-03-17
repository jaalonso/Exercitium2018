% Camino_de_maxima_suma_en_una_matriz.lhs
% Camino de máxima suma en una matriz.
% José A. Alonso Jiménez 
% Sevilla, 8 de marzo de 2018
% ---------------------------------------------------------------------

\epigraph
 {\textit{Caminante, no hay camino, \\
     sino estelas en la mar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Camino_de_maxima_suma_en_una_matriz where
\end{code}
\end{comment}

Los caminos desde el extremo superior izquierdo (posición (1,1))
hasta el extremo inferior derecho (posición (3,4)) en la matriz
\begin{descripcion} 
  (  1  6 11  2 )
  (  7 12  3  8 )
  (  3  8  4  9 )
\end{descripcion}   
moviéndose en cada paso una casilla hacia abajo o hacia la derecha,
son los siguientes:
\begin{descripcion} 
  1, 7,  3, 8, 4, 9
  1, 7, 12, 8, 4, 9
  1, 7, 12, 3, 4, 9
  1, 7, 12, 3, 8, 9
  1, 6, 12, 8, 4, 9
  1, 6, 12, 3, 4, 9
  1, 6, 12, 3, 8, 9
  1, 6, 11, 3, 4, 9
  1, 6, 11, 3, 8, 9
  1, 6, 11, 2, 8, 9
\end{descripcion}   
Las sumas de los caminos son 32, 41, 36, 40, 40, 35, 39, 34, 38 y 37,
respectivamente. El camino de máxima suma es el segundo (1, 7, 12, 8,
4, 9) que tiene una suma de 41.

Definir la función
\begin{descripcion} 
  caminoMaxSuma :: Matrix Int -> [Int]
\end{descripcion}   
tal que (caminoMaxSuma m) es un camino de máxima suma en la matriz m
desde el extremo superior izquierdo hasta el extremo inferior
derecho, moviéndose en cada paso una casilla hacia abajo o hacia la 
derecha. Por ejemplo,
\begin{descripcion} 
  λ> caminoMaxSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
  [1,7,12,8,4,9]
  λ> sum (caminoMaxSuma (fromList 800 800 [1..]))
  766721999
\end{descripcion} 

\section*{Soluciones}

\begin{code}    
import Data.Matrix
import Test.QuickCheck

-- 1ª definición
-- =============

caminoMaxSuma1 :: Matrix Int -> [Int]
caminoMaxSuma1 m =
  head [c | c <- cs, sum c == k] 
  where cs = caminos1 m
        k  = maximum (map sum cs)

caminos1 :: Matrix Int -> [[Int]]
caminos1 m =
  map reverse (caminos1Aux m (nf,nc))
  where nf = nrows m
        nc = ncols m

-- (caminos1Aux p x) es la lista de los caminos invertidos en la matriz p
-- desde la posición (1,1) hasta la posición x. Por ejemplo,
caminos1Aux :: Matrix Int -> (Int,Int) -> [[Int]]
caminos1Aux m (1,1) = [[m!(1,1)]]
caminos1Aux m (1,j) = [[m!(1,k) | k <- [j,j-1..1]]]
caminos1Aux m (i,1) = [[m!(k,1) | k <- [i,i-1..1]]]
caminos1Aux m (i,j) = [m!(i,j) : xs
                      | xs <- caminos1Aux m (i,j-1) ++
                              caminos1Aux m (i-1,j)]

-- 2ª definición
-- =============

caminoMaxSuma2 :: Matrix Int -> [Int]
caminoMaxSuma2 m =
  head [c | c <- cs, sum c == k] 
  where cs = caminos2 m
        k  = maximum (map sum cs)

caminos2 :: Matrix Int -> [[Int]]
caminos2 m =
  map reverse (matrizCaminos m ! (nrows m, ncols m))

matrizCaminos :: Matrix Int -> Matrix [[Int]]
matrizCaminos m = q
  where
    q = matrix (nrows m) (ncols m) f
    f (1,y) = [[m!(1,z) | z <- [y,y-1..1]]]
    f (x,1) = [[m!(z,1) | z <- [x,x-1..1]]]
    f (x,y) = [m!(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]  

-- 3ª definición (con programación dinámica)
-- =========================================

caminoMaxSuma3 :: Matrix Int -> [Int]
caminoMaxSuma3 m = reverse (snd (q ! (nf,nc)))
  where nf = nrows m
        nc = ncols m
        q  = caminoMaxSumaAux m

caminoMaxSumaAux :: Matrix Int -> Matrix (Int,[Int])
caminoMaxSumaAux m = q 
  where
    nf = nrows m
    nc = ncols m
    q  = matrix nf nc f
      where
        f (1,1) = (m!(1,1),[m!(1,1)])
        f (1,j) = (k + m!(1,j), m!(1,j):xs)
          where (k,xs) = q!(1,j-1)
        f (i,1) = (k + m!(i,1), m!(i,1):xs)
          where (k,xs) = q!(i-1,1)        
        f (i,j) | k1 > k2   = (k1 + m!(i,j), m!(i,j):xs)
                | otherwise = (k2 + m!(i,j), m!(i,j):ys)
          where (k1,xs) = q!(i,j-1)
                (k2,ys) = q!(i-1,j)

-- Equivalencia de las definiciones
-- ================================

-- El generador es
instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary =  do
    m <- choose (1,7)
    n <- choose (1,7)
    xs <- Test.QuickCheck.vector (n*m)
    return (fromList m n xs)


-- Por ejemplo,
--    λ> sample' (arbitrary :: Gen (Matrix Int))
--    [( 0 )
--     ( 0 )
--     ( 0 )
--     ( 0 )
--    ,(  1  2 )
--     ( -1  1 )
--     ( -1 -2 )
--     (  1 -1 )
--     (  1  0 )
--     (  2  0 )
--     (  2 -2 )
--    ,( -4  4 -2 )
--     ( -2  0 -2 )
--     (  0 -1 -2 )
--     ( -4 -1  2 )
--    ,( -2  7 -3  1 -5 -3  5 )
--     (  0  2  7 -1 -5  7 -6 )
--     (  1  7 -8  1  6 -7  5 )
--     ( -4  7 -2 -7 -5  5 -8 )
--    ...

-- La propiedad es
prop_caminoMaxSuma :: Matrix Int -> Bool
prop_caminoMaxSuma m =
  x1 == x2 && x2 == x3
  where x1 = sum (caminoMaxSuma1 m)
        x2 = sum (caminoMaxSuma2 m)
        x3 = sum (caminoMaxSuma1 m)

-- La comprobación es
--    λ> quickCheck prop_caminoMaxSuma
--    +++ OK, passed 100 tests.


-- Comparación de eficiencia
-- -------------------------

--    λ> length (caminoMaxSuma1 (fromList 11 11 [1..]))
--    21
--    (10.00 secs, 1,510,120,328 bytes)
--    λ> length (caminoMaxSuma2 (fromList 11 11 [1..]))
--    21
--    (3.84 secs, 745,918,544 bytes)
--    λ> length (caminoMaxSuma3 (fromList 11 11 [1..]))
--    21
--    (0.01 secs, 0 bytes)
\end{code} 
