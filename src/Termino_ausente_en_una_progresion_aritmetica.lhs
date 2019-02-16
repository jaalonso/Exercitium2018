% Termino_ausente_en_una_progresion_aritmetica.lhs
% Término ausente en una progresión aritmética.
% José A. Alonso Jiménez
% Sevilla, 8 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¡Y esa gran placentería \\
     de ruiseñores que cantan! \\
     Ninguna voz es la mía.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Termino_ausente_en_una_progresion_aritmetica where
\end{code}
\end{comment}

Una progresión aritmética es una sucesión de números tales que la
diferencia de dos términos sucesivos cualesquiera de la sucesión es
constante.

Definir la función
\begin{descripcion} 
  ausente :: Integral a => [a] -> a
\end{descripcion} 
tal que (ausente xs) es el único término ausente de la progresión
aritmética xs. Por ejemplo,
\begin{descripcion} 
  ausente [3,7,9,11]               ==  5
  ausente [3,5,9,11]               ==  7
  ausente [3,5,7,11]               ==  9
  ausente ([1..9]++[11..])         ==  10
  ausente ([1..10^6] ++ [2+10^6])  ==  1000001
\end{descripcion}

\textbf{Nota}. Se supone que la lista tiene al menos 3 elementos, que puede
ser infinita y que sólo hay un término de la progresión aritmética
que no está en la lista. 

\section*{Soluciones}

\begin{code} 
import Data.List (group, genericLength)

-- 1ª solución
ausente :: Integral a => [a] -> a
ausente (x1:xs@(x2:x3:_))
  | d1 == d2     = ausente xs
  | d1 == 2 * d2 = x1 + d2
  | d2 == 2 * d1 = x2 + d1
  where d1 = x2 - x1
        d2 = x3 - x2          

-- 2ª solución
ausente2 :: Integral a => [a] -> a
ausente2 s@(x1:x2:x3:_) 
  | x1 + x3 /= 2 * x2 = x1 + (x3 - x2)
  | otherwise         = head [a | (a,b) <- zip [x1,x2..] s
                                , a /= b]

-- 3ª solución
ausente3 :: Integral a => [a] -> a
ausente3  xs@(x1:x2:_) 
  | null us   = x1 + v
  | otherwise = x2 + u * genericLength (u:us) 
  where ((u:us):(v:_):_) = group (zipWith (-) (tail xs) xs)

-- Comparación de eficiencia
--    ghci> let n = 10^6 in ausente1 ([1..n] ++ [n+2])
--    1000001
--    (3.53 secs, 634729880 bytes)
--    
--    ghci> let n = 10^6 in ausente2 ([1..n] ++ [n+2])
--    1000001
--    (0.86 secs, 346910784 bytes)
--    
--    ghci> let n = 10^6 in ausente3 ([1..n] ++ [n+2])
--    1000001
--    (1.22 secs, 501521888 bytes)
--    
--    ghci> let n = 10^7 in ausente2 ([1..n] ++ [n+2])
--    10000001
--    (8.68 secs, 3444142568 bytes)
--    
--    ghci> let n = 10^7 in ausente3 ([1..n] ++ [n+2])
--    10000001
--    (12.59 secs, 4975932088 bytes)
\end{code} 
