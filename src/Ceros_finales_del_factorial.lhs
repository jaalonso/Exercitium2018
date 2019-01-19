% Ceros_finales_del_factorial.hs
% Ceros finales del factorial.
% José A. Alonso Jiménez
% Sevilla, 23 de noviembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{El escepticismo que, lejos de ser, como muchos creen,
    un afán de negarlo todo es, por el contrario, el único medio de
    defender algunas cosas.}} {Antonio Machado}


\begin{comment}
\begin{code}
module Ceros_finales_del_factorial where
\end{code}
\end{comment}

\section*{Enunciado}

Definir la función
\begin{descripcion}
  cerosDelFactorial :: Integer -> Integer
\end{descripcion}
tal que (cerosDelFactorial n) es el número de ceros en que termina el
factorial de n. Por ejemplo,
\begin{descripcion}
  cerosDelFactorial 24                           ==  4
  cerosDelFactorial 25                           ==  6
  length (show (cerosDelFactorial (1234^5678)))  ==  17552
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (genericLength)

-- 1ª definición
-- =============

cerosDelFactorial :: Integer -> Integer
cerosDelFactorial n = ceros (factorial n)

-- (factorial n) es el factorial n. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial n = product [1..n]

-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros :: Integer -> Integer
ceros n | rem n 10 /= 0 = 0
        | otherwise     = 1 + ceros (div n 10)

-- 2ª definición
-- =============

cerosDelFactorial2 :: Integer -> Integer
cerosDelFactorial2 n = ceros2 (factorial n)

-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros2 :: Integer -> Integer
ceros2 n = genericLength (takeWhile (=='0') (reverse (show n)))

-- 3ª definición
-- =============

cerosDelFactorial3 :: Integer -> Integer
cerosDelFactorial3 n
  | n < 5     = 0
  | otherwise = m + cerosDelFactorial3 m
  where m = n `div` 5

-- Comparación de la eficiencia
--    λ> cerosDelFactorial1 (3*10^4)
--    7498
--    (3.96 secs, 1,252,876,376 bytes)
--    λ> cerosDelFactorial2 (3*10^4)
--    7498
--    (3.07 secs, 887,706,864 bytes)
--    λ> cerosDelFactorial3 (3*10^4)
--    7498
--    (0.03 secs, 9,198,896 bytes)
\end{code}
