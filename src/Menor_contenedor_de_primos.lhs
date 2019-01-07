% Menor_contenedor_de_primos.lhs
% Menor contenedor de primos.
% José A. Alonso Jiménez
% Sevilla, 10 de diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Menor_contenedor_de_primos where    
\end{code}
\end{comment}

El n--ésimo menor contenenedor de primos es el menor número que
contiene como subcadenas los primeros n primos. Por ejemplo, el 6º
menor contenedor de primos es 113257 ya que es el menor que contiene
como subcadenas los 6 primeros primos (2, 3, 5, 7, 11 y 13).

Definir la función
\begin{descripcion}
  menorContenedor :: Int -> Int
\end{descripcion}  
tal que (menorContenedor n) es el n-ésimo menor contenenedor de
primos. Por ejemplo,
\begin{descripcion}
  menorContenedor 1  ==  2
  menorContenedor 2  ==  23
  menorContenedor 3  ==  235
  menorContenedor 4  ==  2357
  menorContenedor 5  ==  112357
  menorContenedor 6  ==  113257
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List           (isInfixOf)
import Data.Numbers.Primes (primes)

-- 1ª solución
-- ===========

menorContenedor :: Int -> Int
menorContenedor n =
  head [x | x <- [2..]
          , and [contenido y x | y <- take n primes]]

contenido :: Int -> Int -> Bool
contenido x y =
  show x `isInfixOf` show y

-- 2ª solución
-- ===========

menorContenedor2 :: Int -> Int
menorContenedor2 n =
  head [x | x <- [2..]
          , all (`contenido` x) (take n primes)]
\end{code}

