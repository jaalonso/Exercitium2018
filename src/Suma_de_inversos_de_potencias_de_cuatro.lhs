% Suma_de_inversos_de_potencias_de_cuatro.hs
% Suma de inversos de potencias de cuatro.
% José A. Alonso Jiménez
% Sevilla, 25 de Noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Suma_de_inversos_de_potencias_de_cuatro where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 27--11--18}

Esta semana se ha publicado en \href{http://bit.ly/2Qg4qVQ}{Twitter} una
demostración visual de que
   $$1/4 + 1/4² + 1/4³ + \dots = 1/3$$
como se muestra en la siguiente imagen
\begin{figure}[h]
  \centering
  \includegraphics[scale=0.5]{../src/Suma_de_inversos_de_potencias_de_cuatro.png}
\end{figure}

Definir las funciones
\begin{descripcion}
  sumaInversosPotenciasDeCuatro :: [Double]
  aproximacion :: Double -> Int
\end{descripcion}
tales que
\begin{itemize}
\item sumaInversosPotenciasDeCuatro es la lista de las suma de la serie
  de los inversos de las potencias de cuatro. Por ejemplo,
\begin{descripcion}  
  λ> take 6 sumaInversosPotenciasDeCuatro
  [0.25,0.3125,0.328125,0.33203125,0.3330078125,0.333251953125]
\end{descripcion}
\item (aproximacion e) es el menor número de términos de la serie
  anterior que hay que sumar para que el valor absoluto de su
  diferencia con 1/3 sea menor que e. Por ejemplo,
\begin{descripcion}  
  aproximacion 0.001  ==  4
  aproximacion 1e-3   ==  4
  aproximacion 1e-6   ==  9
  aproximacion 1e-20  ==  26
  sumaInversosPotenciasDeCuatro !! 26  ==  0.3333333333333333
\end{descripcion}
\end{itemize}

\section*{Soluciones}

\begin{code}
-- 1ª definición
sumaInversosPotenciasDeCuatro :: [Double]
sumaInversosPotenciasDeCuatro =
  [sum [1 / (4^k) | k <- [1..n]] | n <- [1..]]

-- 2ª definición
sumaInversosPotenciasDeCuatro2 :: [Double]
sumaInversosPotenciasDeCuatro2 =
  [1/4*((1/4)^n-1)/(1/4-1) | n <- [1..]]

-- 3ª definición
sumaInversosPotenciasDeCuatro3 :: [Double]
sumaInversosPotenciasDeCuatro3 =
  [(1 - 0.25^n)/3 | n <- [1..]]

-- 1ª solución  
aproximacion :: Double -> Int
aproximacion e =
  length (takeWhile (>=e) es)
  where es = [abs (1/3 - x) | x <- sumaInversosPotenciasDeCuatro2]

-- 2ª solución  
aproximacion2 :: Double -> Int
aproximacion2 e =
  head [n | (x,n) <- zip es [0..]
          , x < e]
  where es = [abs (1/3 - x) | x <- sumaInversosPotenciasDeCuatro2]
\end{code}
