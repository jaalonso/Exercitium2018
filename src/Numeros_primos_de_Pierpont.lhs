% Numeros_primos_de_Pierpont.lhs
% Números primos de Pierpont.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 18 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph{\textit{La memoria es infiel: no sólo borra y confunde, sino
    que, a veces, inventa, para desorientarnos.}}{Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Numeros_primos_de_Pierpont where
\end{code}
\end{comment}

Un \href{http://bit.ly/2QSizbM}{número primo de Pierpont}  es un número
primo de la forma \(2^{u}3^{v}+1\), para \(u\) y \(v\) enteros no negativos.

Definir la sucesión
\begin{descripcion}
  primosPierpont :: [Integer]
\end{descripcion}
tal que sus elementos son los números primos de Pierpont. Por
ejemplo,
\begin{descripcion}
  λ> take 20 primosPierpont
  [2,3,5,7,13,17,19,37,73,97,109,163,193,257,433,487,577,769,1153,1297]
  λ> primosPierpont !! 49
  8503057
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Numbers.Primes (primes, primeFactors)

primosPierpont :: [Integer]
primosPierpont =
  [n | n <- primes
     , primoPierpont n]

primoPierpont :: Integer -> Bool
primoPierpont n =
  primeFactors (n-1) `contenidoEn` [2,3]

-- (contenidoEn xs ys) se verifica si xs está contenido en ys. Por
-- ejemplo,
--    contenidoEn [2,3,2,2,3] [2,3]  ==  True
--    contenidoEn [2,3,2,2,1] [2,3]  ==  False
contenidoEn :: [Integer] -> [Integer] -> Bool
contenidoEn xs ys =
  all (`elem` ys) xs
\end{code}
