% Medias_de_digitos_de_pi.lhs
% Medias de dígitos de pi
% José A. Alonso Jiménez
% Sevilla, 5 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Es el mejor de los buenos \\
     quien sabe que en esta vida \\
     todo es cuestión de medida: \\
     un poco más, algo menos.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Medias_de_digitos_de_pi where
\end{code}
\end{comment}

El fichero \href{http://bit.ly/2HuygTl}{Digitos\_de\_pi.txt} contiene el número
pi con un millón de decimales; es decir,
\[3.1415926535897932384626433832 ... 83996346460422090106105779458151\]

Definir las funciones
\begin{descripcion} 
  mediasDigitosDePi        :: IO [Double]
  graficaMediasDigitosDePi :: Int -> IO ()
\end{descripcion}   
tales que
\begin{itemize}
\item mediasDigitosDePi es la sucesión cuyo n--ésimo elemento es la media
  de los n primeros dígitos de pi. Por ejemplo,
\begin{descripcion}   
  λ> xs <- mediasDigitosDePi
  λ> take 5 xs
  [1.0,2.5,2.0,2.75,4.0]
  λ> take 10 xs
  [1.0,2.5,2.0,2.75,4.0,3.6666666666666665,4.0,4.125,4.0,4.1]
  λ> take 10 <$> mediasDigitosDePi
  [1.0,2.5,2.0,2.75,4.0,3.6666666666666665,4.0,4.125,4.0,4.1]
\end{descripcion} 
\item (graficaMediasDigitosDePi n) dibuja la gráfica de los n primeros
  términos de mediasDigitosDePi. Por ejemplo,
  \begin{itemize}
  \item (graficaMediasDigitosDePi 20) dibuja la Figura \ref{fig:MediasPi1}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Medias_de_digitos_de_pi_20.png}
      \caption{(graficaMediasDigitosDePi 20)}
      \label{fig:MediasPi1}
    \end{figure}
  \item (graficaMediasDigitosDePi 200) dibuja la Figura \ref{fig:MediasPi2}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Medias_de_digitos_de_pi_200.png}
      \caption{(graficaMediasDigitosDePi 200)}
      \label{fig:MediasPi2}
    \end{figure}
  \item (graficaMediasDigitosDePi 2000) dibuja la Figura \ref{fig:MediasPi3}
    \begin{figure}[hp]
      \centering
      \includegraphics[scale=0.5]{../src/Medias_de_digitos_de_pi_2000.png}
      \caption{(graficaMediasDigitosDePi 2000)}
      \label{fig:MediasPi3}
    \end{figure}
  \end{itemize}
\end{itemize}

\section*{Soluciones}

\begin{code}        
import Data.Char (digitToInt)
import Data.List (genericLength, inits)
import Graphics.Gnuplot.Simple ( Attribute (Key, PNG)
                               , plotList )

-- Definición de mediasDigitosDePi
-- ===============================

mediasDigitosDePi :: IO [Double]
mediasDigitosDePi = do
  (_:_:ds) <- readFile "Digitos_de_pi.txt"
  return (medias (digitos ds))

-- (digitos cs) es la lista de los digitos de cs. Por ejempplo,
--    digitos "1415926535"  ==  [1,4,1,5,9,2,6,5,3,5]
digitos :: String -> [Int]
digitos = map digitToInt

-- (medias xs) es la lista de las medias de los segmentos iniciales de
-- xs. Por ejemplo,
--    λ> medias [1,4,1,5,9,2,6,5,3,5]
--    [1.0,2.5,2.0,2.75,4.0,3.6666666666666665,4.0,4.125,4.0,4.1]
medias :: [Int] -> [Double]
medias xs = map media (tail (inits xs))

-- (media xs) es la media aritmética de xs. Por ejemplo,
--    media [1,4,1,5,9]  ==  4.0
media :: [Int] -> Double
media xs = fromIntegral (sum xs) / genericLength xs

-- Definición de graficaMediasDigitosDePi
-- ======================================

graficaMediasDigitosDePi :: Int -> IO ()
graficaMediasDigitosDePi n = do
  xs <- mediasDigitosDePi
  plotList [ Key Nothing
           , PNG ("Medias_de_digitos_de_pi_" ++ show n ++ ".png")
           ]
           (take n xs)
\end{code} 
