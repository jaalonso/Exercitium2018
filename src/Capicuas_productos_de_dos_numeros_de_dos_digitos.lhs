% Capicuas_productos_de_dos_numeros_de_dos_digitos.hs
% Capicúas productos de dos números de dos dígitos.
% José A. Alonso Jiménez
% Sevilla, 16 de noviembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Ayudadme a comprender lo que os digo, y os lo
    explicaré más despacio.}} {Antonio Machado}


\begin{comment}
\begin{code}
module Capicuas_productos_de_dos_numeros_de_dos_digitos where    
\end{code}
\end{comment}

\section*{Enunciado}

El número 9009 es capicúa y es producto de dos números de dos dígitos,
pues 9009 = 91x99. 

Definir la lista
\begin{descripcion}
  capicuasP2N2D :: [Int]
\end{descripcion}
cuyos elementos son los números capicúas que son producto de 2
números de dos dígitos. Por ejemplo,
\begin{descripcion}
  take 5  capicuasP2N2D  ==  [121,242,252,272,323]
  length  capicuasP2N2D  ==  74
  drop 70 capicuasP2N2D  ==  [8008,8118,8448,9009]
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.List (nub, sort)

capicuasP2N2D :: [Int]
capicuasP2N2D = [x | x <- productos, esCapicua x]

-- productos es la lista de números que son productos de 2 números de
-- dos dígitos.   
productos :: [Int]
productos = sort (nub [x*y | x <- [10..99], y <- [x..99]])

-- (esCapicua x) se verifica si x es capicúa.
esCapicua :: Int -> Bool
esCapicua x = xs == reverse xs
  where xs = show x
\end{code}
