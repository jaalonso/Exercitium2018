% Numeros_autodescriptivos.hs
% Números autodescriptivos.
% José A. Alonso Jiménez
% Sevilla, 19 de Noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Numeros_autodescriptivos where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 19--11--18}

Un número n es autodescriptivo cuando para cada posición k de n
(empezando a contar las posiciones a partir de 0), el dígito en la
posición k es igual al número de veces que ocurre k en n. Por
ejemplo, 1210 es autodescriptivo porque tiene 1 dígito igual a "0", 2
dígitos iguales a "1", 1 dígito igual a "2" y ningún dígito igual a
"3". 

Definir la función
\begin{descripcion}
  autodescriptivo :: Integer -> Bool
\end{descripcion}
tal que (autodescriptivo n) se verifica si n es autodescriptivo. Por
ejemplo,
\begin{descripcion}
  λ> autodescriptivo 1210
  True
  λ> [x | x <- [1..100000], autodescriptivo x]
  [1210,2020,21200]
  λ> autodescriptivo 9210000001000
  True
\end{descripcion}

\textbf{Nota}: Se puede usar la función
\href{http://bit.ly/2Q7snyg}{genericLength}.

\section*{Soluciones}

\begin{code}
import Data.List (genericLength)

autodescriptivo :: Integer -> Bool
autodescriptivo n = autodescriptiva (digitos n)

digitos :: Integer -> [Integer]
digitos n = [read [d] | d <- show n]

autodescriptiva :: [Integer] -> Bool
autodescriptiva ns = 
  and [x == ocurrencias k ns | (k,x) <- zip [0..] ns]

ocurrencias :: Integer -> [Integer] -> Integer
ocurrencias x ys = genericLength (filter (==x) ys)
\end{code}
