% Relacion_definida_por_una_particion.hs
% Relación definida por una partición.
% José A. Alonso Jiménez
% Sevilla, 22 de noviembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{No hay lío político que no sea un trueque, una
    confusión de máscaras, un mal ensayo de comedia, en que nadie sabe
    su papel.}} {Antonio Machado}


\section*{Enunciado}

\begin{comment}
\begin{code}
module Relacion_definida_por_una_particion where
\end{code}
\end{comment}

Dos elementos están
\href{http://bit.ly/2Q1FzVD}{relacionados por una partición}
xss si pertenecen al mismo elemento de xss.

Definir la función
\begin{descripcion}
  relacionados :: Eq a => [[a]] -> a -> a -> Bool
\end{descripcion}
tal que (relacionados xss y z) se verifica si los elementos y y z
están relacionados por la partición xss. Por ejemplo,
\begin{descripcion}
  relacionados [[1,3],[2],[9,5,7]] 7 9  ==  True
  relacionados [[1,3],[2],[9,5,7]] 3 9  ==  False
  relacionados [[1,3],[2],[9,5,7]] 4 9  ==  False
\end{descripcion}

\section*{Soluciones}

\begin{code}
-- 1ª definición
-- =============

relacionados :: Eq a => [[a]] -> a -> a -> Bool
relacionados [] _ _ = False
relacionados (xs:xss) y z
  | y `elem` xs = z `elem` xs
  | otherwise   = relacionados xss y z

-- 2ª definición
-- =============

relacionados2 :: Eq a => [[a]] -> a -> a -> Bool
relacionados2 xss y z =
  or [elem y xs && elem z xs | xs <- xss]

-- 3ª definición
-- =============

relacionados3 :: Eq a => [[a]] -> a -> a -> Bool
relacionados3 xss y z =
  or [[y,z] `subconjunto` xs | xs <- xss]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys; es
-- decir, si todos los elementos de xs pertenecen a ys. Por ejemplo,  
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- 4ª definición
-- =============

relacionados4 :: Eq a => [[a]] -> a -> a -> Bool
relacionados4 xss y z =
  any ([y,z] `subconjunto`) xss
\end{code}
