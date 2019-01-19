% Elemento_solitario.hs
% Elemento solitario.
% José A. Alonso Jiménez
% Sevilla, 28 de Noviembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Sube y sube, pero ten \\
  cuidado Nefelibata, \\
  que entre las nubes también, \\
  se puede meter la pata.}} {Antonio Machado}


\begin{comment}
\begin{code}
module Elemento_solitario where
\end{code}
\end{comment}

\section*{Enunciado}

Definir la función
\begin{descripcion}
  solitario :: Ord a => [a] -> a
\end{descripcion}
tal que (solitario xs) es el único elemento que ocurre una vez en la
lista xs (se supone que la lista xs tiene al menos 3 elementos y
todos son iguales menos uno que es el solitario). Por ejemplo,
\begin{descripcion}
  solitario [2,2,7,2]  ==  7
  solitario [2,2,2,7]  ==  7
  solitario [7,2,2,2]  ==  7
  solitario (replicate (2*10^7) 1 ++ [2])  ==  2
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Test.QuickCheck 
import Data.List (group, nub, sort)

-- 1ª definición
-- =============

solitario :: Ord a => [a] -> a
solitario xs =
  head [x | x <- xs
          , cuenta xs x == 1]

cuenta :: Eq a => [a] -> a -> Int
cuenta xs x = length [y | y <- xs
                        , x == y]

-- 2ª definición
-- =============

solitario2 :: Ord a => [a] -> a
solitario2 xs = head (filter (\x -> cuenta2 xs x == 1) xs)

cuenta2 :: Eq a => [a] -> a -> Int
cuenta2 xs x = length (filter (==x) xs)

-- 3ª definición
-- =============

solitario3 :: Ord a => [a] -> a
solitario3 [x] = x
solitario3 (x1:x2:x3:xs)
  | x1 /= x2 && x2 == x3 = x1
  | x1 == x2 && x2 /= x3 = x3
  | otherwise            = solitario3 (x2:x3:xs)
solitario3 _ = error "Imposible"

-- 4ª definición
-- =============

solitario4 :: Ord a => [a] -> a
solitario4 xs 
  | y1 == y2  = last ys
  | otherwise = y1
  where (y1:y2:ys) = sort xs

-- 5ª definición
-- =============

solitario5 :: Ord a => [a] -> a
solitario5 xs | null ys   = y
              | otherwise = z
  where [y:ys,z:_] = group (sort xs)

-- 6ª definición
-- =============

solitario6 :: Ord a => [a] -> a
solitario6 xs =
  head [x | x <- nub xs
          , cuenta xs x == 1]

-- 7ª definición
-- =============

solitario7 :: Ord a => [a] -> a
solitario7 (a:b:xs)
  | a == b        = solitario7 (b:xs)
  | elem a (b:xs) = b
  | elem b (a:xs) = a
solitario7 [_,b]  = b
solitario7 _ = error "Imposible"

-- Equivalencia
-- ============

-- Propiedad de equivalencia
prop_solitario_equiv :: Property
prop_solitario_equiv =
  forAll listaSolitaria (\xs -> solitario xs == solitario2 xs &&
                                solitario xs == solitario3 xs &&
                                solitario xs == solitario4 xs &&
                                solitario xs == solitario5 xs &&
                                solitario xs == solitario6 xs &&
                                solitario xs == solitario7 xs)

-- Generador de listas con al menos 3 elementos y todos iguales menos
-- uno. Por ejemplo,
--    λ> sample listaSolitaria
--    [1,0,0,0,0]
--    [0,0,-1,0,0,0]
--    [4,1,1,1]
--    [6,6,4,6]
--    [8,8,8,8,8,-4,8,8,8,8,8,8]
--    ...
listaSolitaria :: Gen [Int]
listaSolitaria = do
  n <- arbitrary
  m <- arbitrary `suchThat` (\a -> n + a > 2)
  x <- arbitrary
  y <- arbitrary `suchThat` (\a -> a /= x)
  return (replicate n x ++ [y] ++ replicate m x)

-- Comprobación:
--    λ> quickCheck prop_solitario_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia:
--    λ> solitario (replicate (5*10^3) 1 ++ [2])
--    2
--    (5.47 secs, 3,202,688,152 bytes)
--    λ> solitario2 (replicate (5*10^3) 1 ++ [2])
--    2
--    (2.08 secs, 1,401,603,960 bytes)
--    λ> solitario3 (replicate (5*10^3) 1 ++ [2])
--    2
--    (0.04 secs, 3,842,240 bytes)
--    λ> solitario4 (replicate (5*10^3) 1 ++ [2])
--    2
--    (0.02 secs, 1,566,472 bytes)
--    λ> solitario5 (replicate (5*10^3) 1 ++ [2])
--    2
--    (0.01 secs, 927,064 bytes)
--    λ> solitario6 (replicate (5*10^3) 1 ++ [2])
--    2
--    (0.01 secs, 1,604,176 bytes)
--    λ> solitario7 (replicate (5*10^3) 1 ++ [2])
--    2
--    (0.01 secs, 1,923,440 bytes)
--    
--    λ> solitario3 (replicate (5*10^6) 1 ++ [2])
--    2
--    (4.62 secs, 3,720,123,560 bytes)
--    λ> solitario4 (replicate (5*10^6) 1 ++ [2])
--    2
--    (1.48 secs, 1,440,124,240 bytes)
--    λ> solitario5 (replicate (5*10^6) 1 ++ [2])
--    2
--    (1.40 secs, 1,440,125,936 bytes)
--    λ> solitario6 (replicate (5*10^6) 1 ++ [2])
--    2
--    (2.65 secs, 1,480,125,032 bytes)
--    λ> solitario7 (replicate (5*10^6) 1 ++ [2])
--    2
--    (2.21 secs, 1,800,126,224 bytes)
--    
--    λ> solitario5 (2 : replicate (5*10^6) 1)
--    2
--    (1.38 secs, 1,520,127,864 bytes)
--    λ> solitario6 (2 : replicate (5*10^6) 1)
--    2
--    (1.18 secs, 560,127,664 bytes)
--    λ> solitario7 (2 : replicate (5*10^6) 1)
--    2
--    (0.29 secs, 280,126,888 bytes)
\end{code}
