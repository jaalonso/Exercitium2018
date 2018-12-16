% Reconocimiento_de_particiones.hs
% Reconocimiento de particiones.
% José A. Alonso Jiménez
% Sevilla, 21 de noviembre de 2018
% ---------------------------------------------------------------------

\section*{Ejercicio propuesto el 21 de Noviembre de 2018}

\begin{comment}
\begin{code}
module Reconocimiento_de_particiones where
\end{code}
\end{comment}

Una \href{http://bit.ly/2Dw2GB4}{partición} de un conjunto es una división
del mismo en subconjuntos disjuntos no vacíos.

Definir la función
\begin{descripcion}
  esParticion :: Eq a => [[a]] -> Bool
\end{descripcion}
tal que (esParticion xss) se verifica si xss es una partición; es
decir sus elementos son listas no vacías disjuntas. Por ejemplo.
\begin{descripcion}
  esParticion [[1,3],[2],[9,5,7]]  ==  True
  esParticion [[1,3],[2],[9,5,1]]  ==  False
  esParticion [[1,3],[],[9,5,7]]   ==  False
  esParticion [[2,3,2],[4]]        ==  True
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List ((\\), intersect)

-- 1ª definición
-- =============

esParticion :: Eq a => [[a]] -> Bool
esParticion xss =
  [] `notElem` xss &&
  and [disjuntos xs ys | xs <- xss, ys <- xss \\ [xs]] 

disjuntos :: Eq a => [a] -> [a] -> Bool
disjuntos xs ys = null (xs `intersect` ys)

-- 2ª definición
-- =============

esParticion2 :: Eq a => [[a]] -> Bool
esParticion2 []       = True
esParticion2 (xs:xss) =
  not (null xs) &&
  and [disjuntos xs ys | ys <- xss] &&
  esParticion2 xss

-- 3ª definición
-- =============

esParticion3 :: Eq a => [[a]] -> Bool
esParticion3 []       = True
esParticion3 (xs:xss) =
  not (null xs) &&
  all (disjuntos xs) xss &&
  esParticion3 xss

-- Equivalencia
prop_equiv :: [[Int]] -> Bool
prop_equiv xss =
  and [esParticion xss == f xss | f <- [ esParticion2
                                       , esParticion3]]

-- Comprobación
--    λ> quickCheck prop_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia:
--    λ> esParticion [[x] | x <- [1..3000]]
--    True
--    (4.37 secs, 3,527,956,400 bytes)
--    λ> esParticion2 [[x] | x <- [1..3000]]
--    True
--    (1.26 secs, 1,045,792,552 bytes)
--    λ> esParticion3 [[x] | x <- [1..3000]]
--    True
--    (1.30 secs, 1,045,795,272 bytes)
--    λ> esParticion3 [[x] | x <- [1..3000]]
--    True
--    (1.30 secs, 1,045,795,272 bytes)
\end{code}
