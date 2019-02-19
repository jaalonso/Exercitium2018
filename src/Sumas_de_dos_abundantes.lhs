% Sumas_de_dos_abundantes.lhs
% Sucesión de sumas de dos números abundantes.
% José A. Alonso Jiménez
% Sevilla, 12 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Dices que nada se crea? \\
     Alfarero, a tus cacharros. \\
     Haz tu copa y no te importe \\
     si no puedes hacer barro.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Sumas_de_dos_abundantes where
\end{code}
\end{comment}

Un número n es \href{http://bit.ly/1vySpf2}{abundante} si la suma de los
divisores propios de n es mayor que n. El primer número abundante es
el 12 (cuyos divisores propios son 1, 2, 3, 4 y 6 cuya suma es
16). Por tanto, el menor número que es la suma de dos números
abundantes es el 24.

Definir la sucesión
\begin{descripcion} 
  sumasDeDosAbundantes :: [Integer]
\end{descripcion} 
cuyos elementos son los números que se pueden escribir como suma de
dos números abundantes. Por ejemplo,
\begin{descripcion} 
  take 10 sumasDeDosAbundantes  ==  [24,30,32,36,38,40,42,44,48,50]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
sumasDeDosAbundantes :: [Integer]
sumasDeDosAbundantes = [n | n <- [1..], esSumaDeDosAbundantes n]

-- (esSumaDeDosAbundantes n) se verifica si n es suma de dos números
-- abundantes. Por ejemplo,
--    esSumaDeDosAbundantes 24           ==  True
--    any esSumaDeDosAbundantes [1..22]  ==  False
esSumaDeDosAbundantes :: Integer -> Bool
esSumaDeDosAbundantes n = (not . null) [x | x <- xs, n-x `elem` xs] 
  where xs = takeWhile (<n) abundantes

-- abundantes es la lista de los números abundantes. Por ejemplo,
--    take 10 abundantes  ==  [12,18,20,24,30,36,40,42,48,54]
abundantes :: [Integer]
abundantes = [n | n <- [2..], abundante n]

-- (abundante n) se verifica si n es abundante. Por ejemplo,
--    abundante 12  ==  True
--    abundante 11  ==  False
abundante :: Integer -> Bool
abundante n = sum (divisores n) > n

-- (divisores n) es la lista de los divisores propios de n. Por ejemplo,
--    divisores 12  ==  [1,2,3,4,6]
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n `div` 2], n `mod` x == 0]
\end{code} 
