% El_2019_es_semiprimo.lhs
% El 2019 es semiprimo.
% José A. Alonso Jiménez
% Sevilla, 30 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Porque toda visión requiere distancia, no hay manera
    de ver las cosas sin salirse de ellas.}} {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module El_2019_es_semiprimo where
\end{code}
\end{comment}

Un \href{http://bit.ly/1NK8bJ0}{número semiprimo} es un número natural que
es producto de dos números primos no necesariamente distintos. Por
ejemplo, 26 es semiprimo (porque 26 = 2*13) y 49 también lo es
(porque 49 = 7*7). 

Definir las funciones
\begin{descripcion} 
  esSemiprimo :: Integer -> Bool
  semiprimos  :: [Integer]
\end{descripcion}   
tales que
\begin{itemize}
\item (esSemiprimo n) se verifica si n es semiprimo. Por ejemplo,
\begin{descripcion}   
  esSemiprimo 26          ==  True
  esSemiprimo 49          ==  True
  esSemiprimo 8           ==  False
  esSemiprimo 2019        ==  True
  esSemiprimo (21+10^14)  ==  True
\end{descripcion} 
\item semiprimos es la sucesión de números semiprimos. Por ejemplo,
\begin{descripcion}   
  take 10 semiprimos   ==  [4,6,9,10,14,15,21,22,25,26]
  semiprimos !! 579    ==  2019
  semiprimos !! 10000  ==  40886
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.Numbers.Primes 
import Test.QuickCheck

-- 1ª definición de esSemiprimo
-- ============================

esSemiprimo :: Integer -> Bool
esSemiprimo n =
  not (null [x | x <- [n,n-1..2], 
                 primo x,
                 n `mod` x == 0,
                 primo (n `div` x)])

primo :: Integer -> Bool
primo n = [x | x <- [1..n], n `mod` x == 0] == [1,n] 

-- 2ª definición de esSemiprimo
-- ============================

esSemiprimo2 :: Integer -> Bool
esSemiprimo2 n =
  not (null [x | x <- [n-1,n-2..2], 
                 isPrime x,
                 n `mod` x == 0,
                 isPrime (n `div` x)])

-- 3ª definición de esSemiprimo
-- ============================

esSemiprimo3 :: Integer -> Bool
esSemiprimo3 n =
  not (null [x | x <- reverse (takeWhile (<n) primes),
                 n `mod` x == 0,
                 isPrime (n `div` x)])

-- 4ª definición de esSemiprimo
-- ============================

esSemiprimo4 :: Integer -> Bool
esSemiprimo4 n =
  length (primeFactors n) == 2

-- Equivalencia de las definiciones de esSemiprimo
-- ===============================================

-- La propiedad es
prop_esSemiprimo :: Positive Integer -> Bool
prop_esSemiprimo (Positive n) =
  all (== esSemiprimo n) [f n | f <- [ esSemiprimo2
                                     , esSemiprimo3
                                     , esSemiprimo4
                                     ]]

-- La comprobación es
--    λ> quickCheck prop_esSemiprimo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> esSemiprimo 5001
--    True
--    (1.90 secs, 274,450,648 bytes)
--    λ> esSemiprimo2 5001
--    True
--    (0.07 secs, 29,377,016 bytes)
--    λ> esSemiprimo3 5001
--    True
--    (0.01 secs, 1,706,840 bytes)
--    λ> esSemiprimo4 5001
--    True
--    (0.01 secs, 142,840 bytes)
--    
--    λ> esSemiprimo2 100001
--    True
--    (2.74 secs, 1,473,519,064 bytes)
--    λ> esSemiprimo3 100001
--    True
--    (0.09 secs, 30,650,352 bytes)
--    λ> esSemiprimo4 100001
--    True
--    (0.01 secs, 155,200 bytes)
--    
--    λ> esSemiprimo3 10000001
--    True
--    (8.73 secs, 4,357,875,016 bytes)
--    λ> esSemiprimo4 10000001
--    True
--    (0.01 secs, 456,328 bytes)

-- Definición de semiprimos
-- ========================

semiprimos :: [Integer]
semiprimos = filter esSemiprimo4 [4..]
\end{code} 
