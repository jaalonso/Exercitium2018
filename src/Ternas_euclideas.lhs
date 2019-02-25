% Ternas_euclideas.lhs
% Ternas euclídeas.
% José A. Alonso Jiménez
% Sevilla, 18 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Todo pasa y todo queda, \\
     pero lo nuestro es pasar, \\
     pasar haciendo caminos, \\
     caminos sobre la mar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Ternas_euclideas where
\end{code}
\end{comment}

Uno de los problemas planteados por Euclides en los Elementos consiste
en encontrar tres números tales que cada uno de sus productos, dos a
dos, aumentados en la unidad sea un cuadrado perfecto.

Diremos que (x,y,z) es una terna euclídea si es una solución del
problema; es decir, si \(x \leq y \leq z\) y \(xy+1, yz+1\) y \(zx+1\) son
cuadrados. Por ejemplo, (4,6,20) es una terna euclídea ya que
\[4 \times 6+1 = 5^2, 6 \times 20+1 = 11^2, 20 \times4+1 = 9^2\]

Definir la funciones
\begin{descripcion} 
  ternasEuclideas        :: [(Integer,Integer,Integer)]
  esMayorDeTernaEuclidea :: Integer -> Bool
\end{descripcion} 
tales que
\begin{itemize}
\item ternasEuclideas es la lista de las ternas euclídeas. Por ejemplo,
\begin{descripcion}   
  λ> take 7 ternasEuclideas
  [(1,3,8),(2,4,12),(1,8,15),(3,5,16),(4,6,20),(3,8,21),(5,7,24)]
\end{descripcion}   
\item (esMayorDeTernaEuclidea z) se verifica si existen x, y tales que
  (x,y,z) es una terna euclídea. Por ejemplo,
\begin{descripcion}   
  esMayorDeTernaEuclidea 20  ==  True
  esMayorDeTernaEuclidea 22  ==  False
\end{descripcion}   
\end{itemize}

Comprobar con QuickCheck que z es el mayor de una terna euclídea si, y
sólo si, existe un número natural x tal que \(1 < x < z - 1\) y
\(x^2\) es congruente con 1 módulo z.

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

ternasEuclideas :: [(Integer,Integer,Integer)]
ternasEuclideas =
  [(x,y,z) | z <- [1..]
           , y <- [1..z]
           , esCuadrado (y * z + 1)
           , x <- [1..y]
           , esCuadrado (x * y + 1)
           , esCuadrado (z * x + 1)]

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Integer -> Bool
esCuadrado x = (raizEntera x)^2 == x
  where raizEntera :: Integer -> Integer
        raizEntera = floor . sqrt . fromIntegral 

esMayorDeTernaEuclidea :: Integer -> Bool
esMayorDeTernaEuclidea z =
  not (null [(x,y) | y <- [1..z]
                   , esCuadrado (y * z + 1)
                   , x <- [1..y]
                   , esCuadrado (x * y + 1)
                   , esCuadrado (z * x + 1)])

-- La propiedad es
prop_esMayorDeTernaEuclidea :: Positive Integer -> Bool
prop_esMayorDeTernaEuclidea (Positive z) =
  esMayorDeTernaEuclidea z == any (\x -> (x^2) `mod` z == 1) [2..z-2]

-- La comprobación es
--    λ> quickCheck prop_esMayorDeTernaEuclidea
--    +++ OK, passed 100 tests.
\end{code} 
