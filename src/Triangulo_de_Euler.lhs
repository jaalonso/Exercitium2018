% Triangulo_de_Euler.lhs
% Triángulo de Euler.
% José A. Alonso Jiménez
% Sevilla, 29 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Señor San Jerónimo, \\
     suelte usted la piedra \\
     con que se machaca. \\
     Me pegó con ella.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Triangulo_de_Euler where
\end{code}
\end{comment}

El triángulo de Euler se construye a partir de las siguientes
relaciones
\begin{descripcion} 
  A(n,1) = A(n,n) = 1
  A(n,m) = (n-m)A(n-1,m-1) + (m+1)A(n-1,m).
\end{descripcion}   
Sus primeros términos son
\begin{descripcion} 
  1 
  1 1                                                       
  1 4   1                                            
  1 11  11    1                                    
  1 26  66    26    1                             
  1 57  302   302   57     1                    
  1 120 1191  2416  1191   120   1            
  1 247 4293  15619 15619  4293  247   1   
  1 502 14608 88234 156190 88234 14608 502 1 
\end{descripcion}

Definir las siguientes funciones:
\begin{descripcion} 
  numeroEuler        :: Integer -> Integer -> Integer
  filaTrianguloEuler :: Integer -> [Integer]
  trianguloEuler     :: [[Integer]]
\end{descripcion}   
tales que
\begin{itemize}
\item (numeroEuler n k) es el número de Euler A(n,k). Por ejemplo,
\begin{descripcion}   
  numeroEuler 8 3  == 15619
  numeroEuler 20 6 == 21598596303099900
  length (show (numeroEuler 1000 500)) == 2567
\end{descripcion}
\item (filaTrianguloEuler n) es la n-ésima fila del triángulo de
  Euler. Por ejemplo,
\begin{descripcion}   
  filaTrianguloEuler 7  ==  [1,120,1191,2416,1191,120,1]
  filaTrianguloEuler 8  ==  [1,247,4293,15619,15619,4293,247,1]
  length (show (maximum (filaTrianguloEuler 1000)))  ==  2567
\end{descripcion} 
\item trianguloEuler es la lista con las filas del triángulo de Euler
\begin{descripcion}   
  λ> take 6 trianguloEuler
  [[1],[1,1],[1,4,1],[1,11,11,1],[1,26,66,26,1],[1,57,302,302,57,1]]
  λ> length (show (maximum (trianguloEuler !! 999)))
  2567
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List  (genericLength, genericIndex)
import Data.Array (Array, (!), array)

-- 1ª solución
-- ===========

trianguloEuler :: [[Integer]]
trianguloEuler = iterate siguiente [1]

-- (siguiente xs) es la fila siguiente a la xs en el triángulo de
-- Euler. Por ejemplo,
--    λ> siguiente [1]
--    [1,1]
--    λ> siguiente it
--    [1,4,1]
--    λ> siguiente it
--    [1,11,11,1]
siguiente :: [Integer] -> [Integer]
siguiente xs = zipWith (+) us vs
  where n = genericLength xs
        us = zipWith (*) (0:xs) [n+1,n..1]
        vs = zipWith (*) (xs++[0]) [1..n+1]

filaTrianguloEuler :: Integer -> [Integer]
filaTrianguloEuler n = trianguloEuler `genericIndex` (n-1)

numeroEuler :: Integer -> Integer -> Integer
numeroEuler n k = filaTrianguloEuler n `genericIndex` k

-- 2ª solución
-- ===========

numeroEuler2 :: Integer -> Integer -> Integer
numeroEuler2 _ 0 = 1
numeroEuler2 n m 
  | n == m    = 0
  | otherwise = (n-m) * numeroEuler2 (n-1) (m-1) + (m+1) * numeroEuler2 (n-1) m

filaTrianguloEuler2 :: Integer -> [Integer]
filaTrianguloEuler2 n = map (numeroEuler2 n) [0..n-1]

trianguloEuler2 :: [[Integer]]
trianguloEuler2 = map filaTrianguloEuler2 [1..]
                  
-- 3ª solución
-- ===========

numeroEuler3 :: Integer -> Integer -> Integer
numeroEuler3 n k = (matrizEuler n k) ! (n,k)

-- (matrizEuler n m) es la matriz de n+1 filas y m+1 columnsa formada
-- por los números de Euler. Por ejemplo,
--   λ> [[matrizEuler 6 6 ! (i,j) | j <- [0..i-1]] | i <- [1..6]]
--   [[1],[1,1],[1,4,1],[1,11,11,1],[1,26,66,26,1],[1,57,302,302,57,1]]
matrizEuler :: Integer -> Integer -> Array (Integer,Integer) Integer
matrizEuler n m = q
  where q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
        f _ 0 = 1
        f i j
          | i == j    = 0
          | otherwise = (i-j) * q!(i-1,j-1) + (j+1)* q!(i-1,j)

filaTrianguloEuler3 :: Integer -> [Integer]
filaTrianguloEuler3 n = map (numeroEuler3 n) [0..n-1]

trianguloEuler3 :: [[Integer]]
trianguloEuler3 = map filaTrianguloEuler3 [1..]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> numeroEuler 22 11
--   301958232385734088196
--   (0.01 secs, 118,760 bytes)
--   λ> numeroEuler2 22 11
--   301958232385734088196
--   (3.96 secs, 524,955,384 bytes)
--   λ> numeroEuler3 22 11
--   301958232385734088196
--   (0.01 secs, 356,296 bytes)
--   
--   λ> length (show (numeroEuler 800 400))
--   1976
--   (0.01 secs, 383,080 bytes)
--   λ> length (show (numeroEuler3 800 400))
--   1976
--   (2.13 secs, 508,780,696 bytes)
\end{code} 
