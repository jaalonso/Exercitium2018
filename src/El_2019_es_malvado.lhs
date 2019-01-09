% El_2019_es_malvado.lhs
% El 2019 es malvado.
% José A. Alonso Jiménez
% Sevilla, 2 de enero de 2019
% ---------------------------------------------------------------------

\epigraph {\textit{\dots Yo os enseño, o pretendo enseñaros a que dudéis de
    todo: de lo humano y de lo divino, sin excluir vuestra propia
    existencia. }} {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module El_2019_es_malvado where
\end{code}
\end{comment}

Un número malvado es un número natural cuya expresión en base 2
contiene un número par de unos. Por ejemplo, 6 es malvado porque su
expresión en base 2 es 110 que tiene dos unos.

Definir las funciones
\begin{descripcion} 
  esMalvado       :: Integer -> Bool
  malvados        :: [Integer]
  posicionMalvada :: Integer -> Maybe Int
\end{descripcion}   
tales que
\begin{itemize}
\item (esMalvado n) se verifica si n es un número malvado. Por ejemplo,
\begin{descripcion}   
  esMalvado 6              ==  True
  esMalvado 7              ==  False
  esMalvado 2019           ==  True
  esMalvado (10^70000)     ==  True
  esMalvado (10^(3*10^7))  ==  True
\end{descripcion}  
\item malvados es la sucesión de los números malvados. Por ejemplo,
\begin{descripcion}   
  λ> take 20 malvados
  [0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39]
  malvados !! 1009    ==  2019
  malvados !! 10      ==  20
  malvados !! (10^2)  ==  201
  malvados !! (10^3)  ==  2000
  malvados !! (10^4)  ==  20001
  malvados !! (10^5)  ==  200000
  malvados !! (10^6)  ==  2000001
\end{descripcion} 
\item (posicionMalvada n) es justo la posición de n en la sucesión de
  números malvados, si n es malvado o Nothing, en caso contrario. Por
  ejemplo,
\begin{descripcion}   
  posicionMalvada 6        ==  Just 3
  posicionMalvada 2019     ==  Just 1009
  posicionMalvada 2018     ==  Nothing
  posicionMalvada 2000001  ==  Just 1000000
  posicionMalvada (10^7)   ==  Just 5000000
\end{descripcion} 
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (genericLength, elemIndex)
import Data.Bits (popCount)

-- 1ª definición de esMalvado
-- ==========================

esMalvado :: Integer -> Bool
esMalvado n = even (numeroUnosBin n)

-- Sin argumentos
esMalvado' :: Integer -> Bool
esMalvado' = even . numeroUnosBin

-- (numeroUnosBin n) es el número de unos de la representación binaria
-- del número decimal n. Por ejemplo,
--   numeroUnosBin 11  ==  3
--   numeroUnosBin 12  ==  2
numeroUnosBin :: Integer -> Integer
numeroUnosBin n  = genericLength (filter (== 1) (binario n))

-- Sin argumentos
numeroUnosBin' :: Integer -> Integer
numeroUnosBin' = genericLength . filter (== 1) . binario

-- (binario n) es el número binario correspondiente al número decimal n.
-- Por ejemplo, 
--   binario 11  ==  [1,1,0,1]
--   binario 12  ==  [0,0,1,1]
binario :: Integer -> [Integer]
binario n | n < 2     = [n]
          | otherwise = n `mod` 2 : binario (n `div` 2)

-- 2ª definición de esMalvado
-- ==========================

esMalvado2 :: Integer -> Bool
esMalvado2 n = even (numeroUnosBin n)

-- (numeroIntBin n) es el número de unos que contiene la representación
-- binaria del número decimal n. Por ejemplo,
--   numeroIntBin 11  ==  3
--   numeroIntBin 12  ==  2
numeroIntBin :: Integer -> Integer
numeroIntBin n | n < 2     = n
               | otherwise = n `mod` 2 + numeroIntBin (n `div` 2)

-- 3ª definición de esMalvado
-- ==========================

esMalvado3 :: Integer -> Bool
esMalvado3 n = even (popCount n)

-- Sin argumentos
esMalvado3' :: Integer -> Bool
esMalvado3' = even . popCount 

-- Comparación de eficiencia
-- =========================

--    λ> esMalvado (10^30000)
--    True
--    (1.79 secs, 664,627,936 bytes)
--    λ> esMalvado2 (10^30000)
--    True
--    (1.79 secs, 664,626,992 bytes)
--    λ> esMalvado3 (10^30000)
--    True
--    (0.03 secs, 141,432 bytes)
--    
--    λ> esMalvado (10^40000)
--    False
--    (2.95 secs, 1,162,091,464 bytes)
--    λ> esMalvado2 (10^40000)
--    False
--    (2.96 secs, 1,162,091,096 bytes)
--    λ> esMalvado3 (10^40000)
--    False
--    (0.04 secs, 155,248 bytes)

-- 1ª definición de malvados
-- =========================

malvados :: [Integer]
malvados = [n | n <- [0..], esMalvado3 n]

-- 2ª definición de malvados
-- =========================

malvados2 :: [Integer]
malvados2 = filter esMalvado3 [0..]

-- 1ª definición de posicionMalvada
-- ================================

posicionMalvada :: Integer -> Maybe Int
posicionMalvada n
  | y == n    = Just (length xs)
  | otherwise = Nothing
  where (xs,y:_) = span (<n) malvados

-- 2ª definición de posicionMalvada
posicionMalvada2 :: Integer -> Maybe Int
posicionMalvada2 n
  | esMalvado n = elemIndex n malvados
  | otherwise        = Nothing
\end{code} 
