% La_sucesion_ECG.lhs
% La sucesión ECG
% José A. Alonso Jiménez
% Sevilla, 21 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Algunos desesperados \\
     sólo se curan con soga; \\
     otros, con siete palabras: \\
     la fe se ha puesto de moda.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module La_sucesion_ECG where
\end{code}
\end{comment}

La sucesión ECG estás definida por a(1) = 1, a(2) = 2 y, para n >= 3,
a(n) es el menor natural que auún no está en la sucesión tal que a(n)
tiene algún divisor común con a(n-1). 

Los primeros términos de la sucesión son 1, 2, 4, 6, 3, 9, 12, 8, 10,
5, 15, ... 

Al dibujar su gráfica, se parece a la de los electrocardiogramas
(abreviadamente, ECG). Por ello, la sucesión se conoce como la
sucesión ECG.

Definir las funciones
\begin{descripcion} 
  sucECG :: [Integer]
  graficaSucECG :: Int -> IO ()
\end{descripcion}
tales que
\begin{itemize}
\item sucECG es la lista de los términos de la sucesión ECG. Por
  ejemplo,
\begin{descripcion}   
  λ> take 20 sucECG
  [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
  λ> sucECG !! 6000
  6237
\end{descripcion} 
\item (graficaSucECG n) dibuja la gráfica de los n primeros términos de
  la sucesión ECG. Por ejemplo, (graficaSucECG 160) dibuja la Figura
  \ref{fig:ECG} 
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/La_sucesion_ECG.png}
      \caption{(graficaSucECG 160)}
      \label{fig:ECG}
    \end{figure}
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (delete)
import Graphics.Gnuplot.Simple

sucECG :: [Integer]
sucECG = 1 : ecg 2 [2..]
  where ecg x zs = f zs
          where f (y:ys) | gcd x y > 1 = y : ecg y (delete y zs)
                         | otherwise   = f ys

graficaSucECG :: Int -> IO ()
graficaSucECG n =
  plotList [ Key Nothing
           , PNG "La_sucesion_ECG.png" 
           ]
           (take n sucECG)
\end{code} 
