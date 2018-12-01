%  Distancia_de_Hamming.hs
%  Distancia de Hamming.
%  José A. Alonso Jiménez
%  Sevilla, 12 de Noviembre de 2018
%  ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 10--11-18}

\begin{comment}
\begin{code}
module Distancia_de_Hamming where
\end{code}
\end{comment}

La distancia de Hamming entre dos listas es el número de posiciones
en que los correspondientes elementos son distintos. Por ejemplo, la
distancia de Hamming entre "roma" y "loba" es 2 (porque hay 2
posiciones en las que los elementos correspondientes son distintos:
la 1ª y la 3ª).  
   
Definir la función
\begin{descripcion}
  distancia :: Eq a => [a] -> [a] -> Int
\end{descripcion}
tal que (distancia xs ys) es la distancia de Hamming entre xs e
ys. Por ejemplo,
\begin{descripcion}
  distancia "romano" "comino"  ==  2
  distancia "romano" "camino"  ==  3
  distancia "roma"   "comino"  ==  2
  distancia "roma"   "camino"  ==  3
  distancia "romano" "ron"     ==  1
  distancia "romano" "cama"    ==  2
  distancia "romano" "rama"    ==  1
\end{descripcion}

Comprobar con QuickCheck si la distancia de Hamming tiene la
siguiente propiedad: distancia(xs,ys) = 0 si, y sólo si, xs = ys
y, en el caso de que no se verifique, modificar ligeramente la
propiedad para obtener una condición necesaria y suficiente de 
distancia(xs,ys) = 0.

\section*{Soluciones}

\begin{code}
import Test.QuickCheck

-- 1ª definición:
distancia :: Eq a => [a] -> [a] -> Int
distancia xs ys = length [(x,y) | (x,y) <- zip xs ys, x /= y] 

-- 2ª definición:
distancia2 :: Eq a => [a] -> [a] -> Int
distancia2 [] _ = 0
distancia2 _ [] = 0
distancia2 (x:xs) (y:ys) | x /= y    = 1 + distancia2 xs ys
                         | otherwise = distancia2 xs ys

-- La propiedad es
prop_distancia1 :: [Int] -> [Int] -> Bool
prop_distancia1 xs ys =
  (distancia xs ys == 0) == (xs == ys)

-- La comprobación es
--    ghci> quickCheck prop_distancia1
--    *** Failed! Falsifiable (after 2 tests and 1 shrink): 
--    []
--    [1]
-- 
-- En efecto,
--    ghci> distancia [] [1] == 0
--    True
--    ghci> [] == [1]
--    False
-- 
-- La primera modificación es restringir la propiedad a lista de igual
-- longitud: 
prop_distancia2 :: [Int] -> [Int] -> Property
prop_distancia2 xs ys =
  length xs == length ys ==> 
  (distancia xs ys == 0) == (xs == ys)

-- La comprobación es
--    ghci> quickCheck prop_distancia2
--    *** Gave up! Passed only 33 tests.

-- Nota. La propiedad se verifica, pero al ser la condición demasiado
-- restringida sólo 33 de los casos la cumple.

-- La segunda restricción es limitar las listas a la longitud de la más
-- corta: 
prop_distancia3 :: [Int] -> [Int] -> Bool
prop_distancia3 xs ys =
  (distancia xs ys == 0) == (take n xs == take n ys)
  where n = min (length xs) (length ys)

-- La comprobación es
--    ghci> quickCheck prop_distancia3
--    +++ OK, passed 100 tests.
\end{code}
