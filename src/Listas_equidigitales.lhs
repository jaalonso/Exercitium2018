% Listas_equidigitales.hs
% Listas equidigitales.
% José A. Alonso Jiménez
% Sevilla, 9 de noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Listas_equidigitales where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 9 de noviembre de 2018}

Una lista de números naturales es equidigital si todos sus elementos
tienen el mismo número de dígitos.

Definir la función
\begin{descripcion}
  equidigital :: [Int] -> Bool
\end{descripcion}
tal que \verb|(equidigital xs)| se verifica si \verb|xs| es una lista
equidigital.  Por ejemplo,
\begin{descripcion}
  equidigital [343,225,777,943]   ==  True
  equidigital [343,225,777,94,3]  ==  False
\end{descripcion}

\section*{Soluciones}

\begin{code}
-- 1ª definición
-- =============

equidigital :: [Int] -> Bool
equidigital xs = todosIguales (numerosDeDigitos xs)

-- (numerosDeDigitos xs) es la lista de los números de dígitos de
-- los elementos de xs. Por ejemplo, 
--    numerosDeDigitos [343,225,777,943]   ==  [3,3,3,3]
--    numerosDeDigitos [343,225,777,94,3]  ==  [3,3,3,2,1]
numerosDeDigitos :: [Int] -> [Int]
numerosDeDigitos xs = [numeroDeDigitos x | x <- xs]

-- (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
--    numeroDeDigitos 475  ==  3
numeroDeDigitos :: Int -> Int
numeroDeDigitos x = length (show x)

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [3,3,3,3]    ==  True
--    todosIguales [3,3,3,2,1]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales (x:y:zs) = x == y && todosIguales (y:zs)
todosIguales _        = True

-- 2ª definición
-- =============

equidigital2 :: [Int] -> Bool
equidigital2 []     = True
equidigital2 (x:xs) = and [numeroDeDigitos y == n | y <- xs]
    where n = numeroDeDigitos x

-- 3ª definición
-- =============

equidigital3 :: [Int] -> Bool
equidigital3 (x:y:zs) = numeroDeDigitos x == numeroDeDigitos y &&
                        equidigital3 (y:zs)
equidigital3 _        = True
\end{code}
