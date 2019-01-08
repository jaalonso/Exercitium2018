% El_2019_es_apocaliptico.lhs
% El 2019 es apocalíptico.
% José A. Alonso Jiménez
% Sevilla, 30 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{A vosotros no os importe pensar lo que habéis leído
    ochenta veces y oído quinientas, porque no es lo mismo pensar que
    haber leído.}}  {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module El_2019_es_apocaliptico where
\end{code}
\end{comment}

Un número natural n es [apocalíptico](http://bit.ly/2RqeeNk) si \(2^n\)
contiene la secuencia 666. Por ejemplo, 157 es apocalíptico porque
\(2^157\) es
\[182687704666362864775460604089535377456991567872\]
que contiene la secuencia 666.

Definir las funciones
\begin{descripcion} 
  esApocaliptico       :: Integer -> Bool
  apocalipticos        :: [Integer]
  posicionApocaliptica :: Integer -> Maybe Int
\end{descripcion} 
tales que
\begin{itemize}
\item (esApocaliptico n) se verifica si n es un número apocalíptico. Por
  ejemplo,
\begin{descripcion}   
  esApocaliptico 157   ==  True
  esApocaliptico 2019  ==  True
  esApocaliptico 2018  ==  False
\end{descripcion}   
\item apocalipticos es la lista de los números apocalípticos. Por
  ejemplo,
\begin{descripcion}  
  take 9 apocalipticos  ==  [157,192,218,220,222,224,226,243,245]
  apocalipticos !! 450  ==  2019
\end{descripcion}   
\item (posicionApocalitica n) es justo la posición de n en la sucesión de
  números apocalípticos, si n es apocalíptico o Nothing, en caso
  contrario. Por ejemplo,
\begin{descripcion}   
  posicionApocaliptica 157   ==  Just 0
  posicionApocaliptica 2019  ==  Just 450
  posicionApocaliptica 2018  ==  Nothing
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (isInfixOf, elemIndex)

-- 1ª definición de esApocaliptico
esApocaliptico :: Integer -> Bool
esApocaliptico n = "666" `isInfixOf` show (2^n)

-- 2ª definición de esApocaliptico
esApocaliptico2 :: Integer -> Bool
esApocaliptico2 = isInfixOf "666" . show . (2^)

-- 1ª definición de apocalipticos
apocalipticos :: [Integer]
apocalipticos = [n | n <- [1..], esApocaliptico n]

-- 2ª definición de apocalipticos
apocalipticos2 :: [Integer]
apocalipticos2 = filter esApocaliptico [1..]

-- 1ª definición de posicionApocaliptica
posicionApocaliptica :: Integer -> Maybe Int
posicionApocaliptica n
  | y == n    = Just (length xs)
  | otherwise = Nothing
  where (xs,y:_) = span (<n) apocalipticos

-- 2ª definición de posicionApocaliptica
posicionApocaliptica2 :: Integer -> Maybe Int
posicionApocaliptica2 n
  | esApocaliptico n = elemIndex n apocalipticos
  | otherwise        = Nothing
\end{code} 
