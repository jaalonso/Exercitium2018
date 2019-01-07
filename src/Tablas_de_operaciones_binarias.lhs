% Tablas_de_operaciones_binarias.lhs
% Tablas de operaciones binarias.
% José A. Alonso Jiménez
% Sevilla, 26 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Tu verdad? No, la Verdad, \\
          y ven conmigo a buscarla. \\
          La tuya guárdatela.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Tablas_de_operaciones_binarias where
\end{code}
\end{comment}
 
Para representar las operaciones binarias en un conjunto finito A con
n elementos se pueden numerar sus elementos desde el 0 al n-1.
Entonces cada operación binaria en A se puede ver como una lista 
de listas xss tal que el valor de aplicar la operación a los elementos
i y j es el j--ésimo elemento del i--ésimo elemento de xss. Por
ejemplo, si A = {0,1,2} entonces las tabla de la suma y de la resta
módulo 3 en A son
\begin{descripcion} 
  0 1 2    0 2 1
  1 2 0    1 0 2
  2 0 1    2 1 0
  Suma     Resta
\end{descripcion}
 
Definir las funciones
\begin{descripcion}  
  tablaOperacion :: (Int -> Int -> Int) -> Int -> [[Int]]
  tablaSuma      :: Int -> [[Int]]
  tablaResta     :: Int -> [[Int]]
  tablaProducto  :: Int -> [[Int]]
\end{descripcion} 
tales que
\begin{itemize}
\item (tablaOperacion f n) es la tabla de la operación f módulo n en
  [0..n-1]. Por ejemplo,
\begin{descripcion}   
  tablaOperacion (+) 3  ==  [[0,1,2],[1,2,0],[2,0,1]]
  tablaOperacion (-) 3  ==  [[0,2,1],[1,0,2],[2,1,0]]
  tablaOperacion (-) 4  ==  [[0,3,2,1],[1,0,3,2],[2,1,0,3],[3,2,1,0]]
  tablaOperacion (\x y -> abs (x-y)) 3  ==  [[0,1,2],[1,0,1],[2,1,0]]
\end{descripcion} 
\item (tablaSuma n) es la tabla de la suma módulo n en [0..n-1]. Por
  ejemplo,
\begin{descripcion}   
  tablaSuma 3  ==  [[0,1,2],[1,2,0],[2,0,1]]
  tablaSuma 4  ==  [[0,1,2,3],[1,2,3,0],[2,3,0,1],[3,0,1,2]]
\end{descripcion} 
\item (tablaResta n) es la tabla de la resta módulo n en [0..n-1]. Por
  ejemplo,
\begin{descripcion}   
  tablaResta 3  ==  [[0,2,1],[1,0,2],[2,1,0]]
  tablaResta 4  ==  [[0,3,2,1],[1,0,3,2],[2,1,0,3],[3,2,1,0]]
\end{descripcion} 
\item (tablaProducto n) es la tabla del producto módulo n en [0..n-1]. Por
  ejemplo,
\begin{descripcion}   
  tablaProducto 3  ==  [[0,0,0],[0,1,2],[0,2,1]]
  tablaProducto 4  ==  [[0,0,0,0],[0,1,2,3],[0,2,0,2],[0,3,2,1]]
\end{descripcion} 
\end{itemize}

Comprobar con QuickCheck, si parato entero positivo n de verificar
las siguientes propiedades:
\begin{itemize}
\item La suma, módulo n, de todos los números de (tablaSuma n) es 0.
\item La suma, módulo n, de todos los números de (tablaResta n) es 0.
\item La suma, módulo n, de todos los números de (tablaProducto n) es
  n/2 si n es el doble de un número impar y es 0, en caso contrario.
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

tablaOperacion :: (Int -> Int -> Int) -> Int -> [[Int]]
tablaOperacion f n =
  [[f i j `mod` n | j <- [0..n-1]] | i <- [0..n-1]]

tablaSuma :: Int -> [[Int]]
tablaSuma = tablaOperacion (+)

tablaResta :: Int -> [[Int]]
tablaResta = tablaOperacion (-)

tablaProducto :: Int -> [[Int]]
tablaProducto = tablaOperacion (*)

-- (sumaTabla xss) es la suma, módulo n, de los elementos de la tabla de
-- operación xss (donde n es el número de elementos de xss). Por
-- ejemplo, 
--    sumaTabla [[0,2,1],[1,1,2],[2,1,0]]  ==  1
sumaTabla :: [[Int]] -> Int
sumaTabla = sum . concat

-- La propiedad de la tabla de la suma es
prop_tablaSuma :: Positive Int -> Bool
prop_tablaSuma (Positive n) =
  sumaTabla (tablaSuma n) == 0

-- La comprobación es
--    λ> quickCheck prop_tablaSuma
--    +++ OK, passed 100 tests.

-- La propiedad de la tabla de la resta es
prop_tablaResta :: Positive Int -> Bool
prop_tablaResta (Positive n) =
  sumaTabla (tablaResta n) == 0

-- La comprobación es
--    λ> quickCheck prop_tablaResta
--    +++ OK, passed 100 tests.
  
-- La propiedad de la tabla del producto es
prop_tablaProducto :: Positive Int -> Bool
prop_tablaProducto (Positive n)
  | even n && odd (n `div` 2) = suma == n `div` 2
  | otherwise                 = suma == 0
  where suma = sumaTabla (tablaProducto n)
\end{code} 
