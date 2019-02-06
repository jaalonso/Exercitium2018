% Impares_en_filas_del_triangulo_de_Pascal.lhs
% Impares en filas del triángulo de Pascal.
% José A. Alonso Jiménez
% Sevilla, 30 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{De lo que llaman los hombres \\
     virtud, justicia y bondad, \\
     una mitad es envidia, \\
     y la otra no es caridad.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Impares_en_filas_del_triangulo_de_Pascal where
\end{code}
\end{comment}

El triángulo de Pascal es un triángulo de números
\begin{descripcion} 
         1
        1 1
       1 2 1
     1  3 3  1
    1 4  6  4 1
   1 5 10 10 5 1
  ...............
\end{descripcion} 
construido de la siguiente forma
\begin{itemize}
\item la primera fila está formada por el número 1;
\item las filas siguientes se construyen sumando los números adyacentes
  de la fila superior y añadiendo un 1 al principio y al final de la
  fila. 
\end{itemize}

Definir las funciones
\begin{descripcion} 
  imparesPascal          :: [[Integer]]
  nImparesPascal         :: [Int]
  grafica_nImparesPascal :: Int -> IO ()
\end{descripcion}   
tales que
\begin{itemize}
\item imparesPascal es la lista de los elementos impares en cada una de
  las filas del triángulo de Pascal. Por ejemplo,
\begin{descripcion} 
  λ> take 8 imparesPascal
  [[1],
   [1,1],
   [1,1],
   [1,3,3,1],
   [1,1],
   [1,5,5,1],
   [1,15,15,1],
   [1,7,21,35,35,21,7,1]]
\end{descripcion} 
\item nImparesPascal es la lista del número de elementos impares en cada
  una de las filas del triángulo de Pascal. Por ejemplo,
\begin{descripcion} 
  λ> take 30 nImparesPascal
  [1,2,2,4,2,4,4,8,2,4,4,8,4,8,8,16,2,4,4,8,4,8,8,16,4,8,8,16,8,16]
  λ> maximum (take (10^6) nImparesPascal)
  524288
\end{descripcion} 
\item \verb|(grafica_nImparesPascal n)| dibuja la gráfica de los n primeros
  términos de nImparesPascal. Por ejemplo,
  \begin{itemize}
  \item \verb|(grafica_nImparesPascal 50)| dibuja la Figura \ref{fig:ImparesPascal1}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Impares_en_filas_del_triangulo_de_Pascal_50.png}
      \caption{(grafica\_nImparesPascal 50)}
      \label{fig:ImparesPascal1}
    \end{figure}
  \item \verb|(grafica_nImparesPascal 100)| dibuja la Figura \ref{fig:ImparesPascal2}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Impares_en_filas_del_triangulo_de_Pascal_100.png}
      \caption{(grafica\_nImparesPascal 100)}
      \label{fig:ImparesPascal2}
    \end{figure}
  \end{itemize}
\end{itemize}

Comprobar con QuickCheck que todos los elementos de nImparesPascal
son potencias de dos.

\section*{Soluciones}

\begin{code} 
import Data.List (transpose)
import Test.QuickCheck
import Graphics.Gnuplot.Simple

-- 1ª definición de imparesPascal
-- ==============================

imparesPascal :: [[Integer]]
imparesPascal =
  map (filter odd) pascal

-- pascal es la lista de las filas del triángulo de Pascal. Por ejemplo,
--    λ> take 7 pascal
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]
pascal :: [[Integer]]
pascal = [1] : map f pascal
  where f xs = zipWith (+) (0:xs) (xs++[0])

-- 2ª definición de imparesPascal
-- ==============================

imparesPascal2 :: [[Integer]]
imparesPascal2 =
  map (filter odd) pascal

pascal2 :: [[Integer]]
pascal2 = iterate f [1]
  where f xs = zipWith (+) (0:xs) (xs++[0])

-- 1ª definición de nImparesPascal
-- ===============================

nImparesPascal :: [Int]
nImparesPascal =
  map length imparesPascal

-- 2ª definición de nImparesPascal
-- ===============================

nImparesPascal2 :: [Int]
nImparesPascal2 =
  map (length . filter odd) imparesPascal

-- 3ª definición de nImparesPascal
-- ===============================

--    λ> take 32 nImparesPascal2
--    [1,2,
--     2,4,
--     2,4,4,8,
--     2,4,4,8,4,8,8,16,
--     2,4,4,8,4,8,8,16,4,8,8,16,8,16,16,32]
nImparesPascal3 :: [Int]
nImparesPascal3 = 1 : zs
  where zs = 2 : concat (transpose [zs, map (*2) zs])

-- Definición de grafica_nImparesPascal
-- =========================================

grafica_nImparesPascal :: Int -> IO ()
grafica_nImparesPascal n =
  plotListStyle
    [ Key Nothing
    , PNG ("Impares_en_filas_del_triangulo_de_Pascal_" ++ show n ++ ".png")
    ]
    (defaultStyle {plotType = LinesPoints})
    (take n nImparesPascal3)

-- Propiedad de nImparesPascal
-- ===========================

-- La propiedad es
prop_nImparesPascal :: Positive Int -> Bool
prop_nImparesPascal (Positive n) =
  esPotenciaDeDos (nImparesPascal3 !! n)

-- (esPotenciaDeDos n) se verifica si n es una potencia de dos. Por
-- ejemplo,
--    esPotenciaDeDos 16  ==  True
--    esPotenciaDeDos 18  ==  False
esPotenciaDeDos :: Int -> Bool
esPotenciaDeDos 1 = True
esPotenciaDeDos n = even n && esPotenciaDeDos (n `div` 2)

-- La comprobación es
--    λ> quickCheck prop_nImparesPascal
--    +++ OK, passed 100 tests.
\end{code} 
