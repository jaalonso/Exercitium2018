% El_2019_es_un_numero_de_la_suerte.lhs
% El 2019 es un número de la suerte.
% José A. Alonso Jiménez
% Sevilla, 30 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph
 {\textit{Ya es sólo brocal el pozo; \\
          púlpito será mañana; \\
          pasado mañana, trono.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module El_2019_es_un_numero_de_la_suerte where
\end{code}
\end{comment}

Un \href{http://bit.ly/2gG48Sl}{número de la suerte} es un número natural
que se genera por una criba, similar a la criba de Eratóstenes, como
se indica a continuación:

Se comienza con la lista de los números enteros a partir de 1:
\begin{descripcion} 
  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25...
\end{descripcion} 
Se eliminan los números de dos en dos
\begin{descripcion} 
  1,  3,  5,  7,  9,   11,   13,   15,   17,   19,   21,   23,   25...
\end{descripcion}   
Como el segundo número que ha quedado es 3, se eliminan los números
restantes de tres en tres:
\begin{descripcion} 
  1,  3,      7,  9,         13,   15,         19,   21,         25...
\end{descripcion}   
Como el tercer número que ha quedado es 7, se eliminan los números
restantes de siete en siete:
\begin{descripcion} 
  1,  3,      7,  9,         13,   15,               21,         25...
\end{descripcion}

Este procedimiento se repite indefinidamente y los supervivientes son
los números de la suerte:
\begin{descripcion} 
  1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79
\end{descripcion}

Definir las funciones
\begin{descripcion} 
  numerosDeLaSuerte  :: [Int]
  esNumeroDeLaSuerte :: Int -> Bool
\end{descripcion}   
tales que
\begin{itemize}
\item numerosDeLaSuerte es la sucesión de los números de la suerte. Por
  ejemplo,
\begin{descripcion}   
  λ> take 20 numerosDeLaSuerte
  [1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
  λ> numerosDeLaSuerte !! 277
  2019
  λ> numerosDeLaSuerte !! 2000
  19309
\end{descripcion} 
\item (esNumeroDeLaSuerte n) que se verifica si n es un número de la
  suerte. Por ejemplo,
\begin{descripcion}   
  esNumeroDeLaSuerte 15    ==  True
  esNumeroDeLaSuerte 16    ==  False
  esNumeroDeLaSuerte 2019  ==  True
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
-- 1ª definición de numerosDeLaSuerte 
numerosDeLaSuerte :: [Int]
numerosDeLaSuerte = criba 3 [1,3..]
  where
    criba i (n:s:xs) =
      n : criba (i + 1) (s : [x | (k, x) <- zip [i..] xs
                                , rem k s /= 0])
    
    
-- 2ª definición de numerosDeLaSuerte 
numerosDeLaSuerte2 :: [Int]
numerosDeLaSuerte2 =  1 : criba 2 [1, 3..]
  where criba k xs = z : criba (k + 1) (aux xs)
          where z = xs !! (k - 1 )
                aux ws = us ++ aux vs
                  where (us, _:vs) = splitAt (z - 1) ws 

-- Comparación de eficiencia
-- =========================

--    λ> numerosDeLaSuerte2 !! 200
--    1387
--    (9.25 secs, 2,863,983,232 bytes)
--    λ> numerosDeLaSuerte !! 200
--    1387
--    (0.06 secs, 10,263,880 bytes)

-- Definición de esNumeroDeLaSuerte
esNumeroDeLaSuerte :: Int -> Bool
esNumeroDeLaSuerte n =
  n == head (dropWhile (<n) numerosDeLaSuerte)
\end{code} 
