% Aproximacion_entre_pi_y_e.hs
% Aproximación entre pi y e.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 7 de diciembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Aproximacion_entre_pi_y_e where 
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 7 de diciembre de 2018}

El día 11 de noviembre, se publicó en la cuenta de Twitter de
\href{http://bit.ly/2DNfhzS}{Fermat's Library} la siguiente curiosa
identidad que relaciona los números e y pi:
$$
  \frac{1}{\pi^2+ 1} +
    \frac{1}{4\pi^2+1} +
      \frac{1}{9\pi^2+1} +
        \frac{1}{16\pi^2+1} + \dots =
          \frac{1}{e^2-1}
$$

Definir las siguientes funciones:
\begin{descripcion}
   sumaTerminos :: Int -> Double
   aproximacion :: Double -> Int
\end{descripcion}
tales que
\begin{itemize}
\item (sumaTerminos n) es la suma de los primeros n términos de la serie
$$
  \frac{1}{\pi^2+ 1} +
    \frac{1}{4\pi^2+1} +
      \frac{1}{9\pi^2+1} +
        \frac{1}{16\pi^2+1} + \dots =
          \frac{1}{e^2-1}
$$
Por ejemplo,
\begin{descripcion}  
     sumaTerminos 10     ==  0.14687821811081034
     sumaTerminos 100    ==  0.15550948345688423
     sumaTerminos 1000   ==  0.15641637221314514
     sumaTerminos 10000  ==  0.15650751113789382
\end{descripcion}
\item (aproximación x) es el menor número de términos que hay que sumar
  de la serie anterior para que se diferencie (en valor absoluto) de
  $\frac{1}{e^2-1}$ menos que x. Por ejemplo,
\begin{descripcion}  
     aproximacion 0.1     ==  1
     aproximacion 0.01    ==  10
     aproximacion 0.001   ==  101
     aproximacion 0.0001  ==  1013
\end{descripcion}
\end{itemize}

\section*{Soluciones}

\begin{code}
-- 1ª definición de sumaTerminos
sumaTerminos :: Int -> Double
sumaTerminos n =
  sum [1 / (((x ^ 2) * (pi ^ 2)) + 1) | x <- [1 .. fromIntegral n]]

-- 2ª definición de sumaTerminos
sumaTerminos2 :: Int -> Double
sumaTerminos2 0 = 0
sumaTerminos2 n = 1 / (m^2 * pi^2 + 1) + sumaTerminos2 (n-1)
  where m = fromIntegral n

-- Definición de aproximacion
aproximacion :: Double -> Int
aproximacion x =
  head [n | n <- [0..]
          , abs (sumaTerminos n - 1 / (e^2 - 1)) < x]
  where e = exp 1
\end{code}
