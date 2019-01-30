% Sucesion_triangular.lhs
% Sucesión triangular.
% José A. Alonso Jiménez
% Sevilla, 23 de enero de 2019
% ---------------------------------------------------------------------

\epigraph {\textit{Nadie debe asustarse de lo que piensa, aunque su
    pensar aparezca en pugna con las leyes más elementales de la
    lógica. Porque todo ha de ser pensado por alguien, y el mayor
    desatino puede ser un punto de vista de lo real.}}  {Antonio
  Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Sucesion_triangular where
\end{code}
\end{comment}

La sucesión triangular es la obtenida concatenando las listas [1],
[1,2], [1,2,3], [1,2,3,4], ....

Sus primeros términos son 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5,
1, 2, 3, 4, 5, 6

Definir las funciones
\begin{descripcion} 
  sucTriangular        :: [Integer]
  terminoSucTriangular :: Int -> Integer
  graficaSucTriangular :: Int -> IO ()
\end{descripcion} 
tales que
\begin{itemize}
\item sucTriangular es la lista de los términos de la sucesión
  triangular. Por ejemplo,
\begin{descripcion} 
  λ> take 30 sucTriangular
  [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,1,2,3,4,5,6,7,1,2]
\end{descripcion} 
\item (terminoSucTriangular n) es el término n-ésimo de la sucesión
  triangular. Por ejemplo,
\begin{descripcion} 
  terminoSucTriangular 5       ==  3
  terminoSucTriangular 10      ==  1
  terminoSucTriangular 20      ==  6
  terminoSucTriangular 100     ==  10
  terminoSucTriangular 1001    ==  12
  terminoSucTriangular (10^5)  ==  320
\end{descripcion} 
\item (graficaSucTriangular n) dibuja la gráfica de los n primeros
  términos de la sucesión triangular. Por ejemplo, (graficaSucTriangular
  300) dibuja
\begin{figure}[h]
  \centering
  \includegraphics[scale=0.5]{../src/Sucesion_triangular.png}
\end{figure}
\end{itemize}

\section*{Soluciones}

\begin{code}       
import Data.List (inits)
import Test.QuickCheck
import Graphics.Gnuplot.Simple

-- 1ª definición de sucTriangular 
-- ==============================

sucTriangular :: [Integer]
sucTriangular =
  concat [[1..n] | n <- [1..]]

-- 2ª definición de sucTriangular 
-- ==============================

sucTriangular2 :: [Integer]
sucTriangular2 =
  [x | n <- [1..], x <- [1..n]]

-- 3ª definición de sucTriangular 
-- ==============================

sucTriangular3 :: [Integer]
sucTriangular3 =
  concat (tail (inits [1..]))
  
-- 1ª definición de terminoSucTriangular
-- =====================================

terminoSucTriangular :: Int -> Integer
terminoSucTriangular k =
  sucTriangular !! k

-- 2ª definición de terminoSucTriangular
-- =====================================

terminoSucTriangular2 :: Int -> Integer
terminoSucTriangular2 k =
  sucTriangular2 !! k

-- 3ª definición de terminoSucTriangular
-- =====================================

terminoSucTriangular3 :: Int -> Integer
terminoSucTriangular3 k =
  sucTriangular3 !! k

-- Equivalencia de definiciones
-- ============================

-- La propiedad es
prop_terminoTriangular :: Positive Int -> Bool
prop_terminoTriangular (Positive n) =
  terminoSucTriangular n == terminoSucTriangular2 n &&
  terminoSucTriangular n == terminoSucTriangular3 n

-- La comprobación es
--      λ> quickCheck prop_terminoTriangular
--      +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> terminoSucTriangular (3*10^6)
--    2425
--    (2.07 secs, 384,707,936 bytes)
--    λ> terminoSucTriangular2 (3*10^6)
--    2425
--    (2.22 secs, 432,571,208 bytes)
--    λ> terminoSucTriangular3 (3*10^6)
--    2425
--    (0.69 secs, 311,259,504 bytes)

-- Definición de graficaSucTriangular
-- ==================================

graficaSucTriangular :: Int -> IO ()
graficaSucTriangular n =
  plotList [ Key Nothing
           , PNG "Sucesion_triangular.png"
           ]
           (take n sucTriangular)
\end{code} 
