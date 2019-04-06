% Matriz_girada_180_grados.lhs
% Matriz girada 180 grados.
% José A. Alonso Jiménez
% Sevilla, 27 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Bueno es recordar \\
     las palabras viejas \\
     que han de volver a sonar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Matriz_girada_180_grados where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
   matrizGirada180 :: Matrix a -> Matrix a
\end{descripcion} 
tal que (matrizGirada180 p) es la matriz obtenida girando 180 grados la
matriz p. Por ejemplo,
\begin{descripcion} 
   λ> fromList 4 3 [1..]
   (  1  2  3 )
   (  4  5  6 )
   (  7  8  9 )
   ( 10 11 12 )

   λ> matrizGirada180 (fromList 4 3 [1..])
   ( 12 11 10 )
   (  9  8  7 )
   (  6  5  4 )
   (  3  2  1 )

   λ> fromList 3 4 [1..]
   (  1  2  3  4 )
   (  5  6  7  8 )
   (  9 10 11 12 )

   λ> matrizGirada180 (fromList 3 4 [1..])
   ( 12 11 10  9 )
   (  8  7  6  5 )
   (  4  3  2  1 )
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Data.Matrix ( Matrix
                   , (!)
                   , fromList
                   , fromLists
                   , matrix
                   , ncols
                   , nrows
                   , toLists
                   )

-- 1ª solución
matrizGirada180 :: Matrix a -> Matrix a
matrizGirada180 p = matrix m n f
  where m       = nrows p
        n       = ncols p
        f (i,j) = p!(m-i+1,n-j+1)

-- 2ª solución
matrizGirada180b :: Matrix a -> Matrix a
matrizGirada180b p =
  fromLists (reverse (map reverse (toLists p)))

-- 3ª solución
matrizGirada180c :: Matrix a -> Matrix a
matrizGirada180c =
  fromLists . reverse . map reverse . toLists
\end{code} 
