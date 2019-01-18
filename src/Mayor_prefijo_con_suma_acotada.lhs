% Mayor_prefijo_con_suma_acotada.lhs
% Mayor prefijo con suma acotada.
% José A. Alonso Jiménez
% Sevilla, 25 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Sed hombres de mal gusto. Yo os aconsejo el mal gusto
    para combatir los excesos de la moda.}}  {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Mayor_prefijo_con_suma_acotada where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  mayorPrefijoAcotado :: [Int] -> Int -> [Int]
\end{descripcion} 
tal que (mayorPrefijoAcotado xs y) es el mayor prefijo de la lista de
números enteros positivos xs cuya suma es menor o igual que y. Por
ejemplo,
\begin{descripcion} 
  mayorPrefijoAcotado [45,30,55,20,80,20] 75   ==  [45,30]
  mayorPrefijoAcotado [45,30,55,20,80,20] 140  ==  [45,30,55]
  mayorPrefijoAcotado [45,30,55,20,80,20] 180  ==  [45,30,55,20]
  length (mayorPrefijoAcotado (repeat 1) (8*10^6)) == 8000000
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (inits)

-- 1ª solución
-- ===========

mayorPrefijoAcotado :: [Int] -> Int -> [Int]
mayorPrefijoAcotado [] _     = []
mayorPrefijoAcotado (x:xs) y
  | x > y     = []
  | otherwise = x : mayorPrefijoAcotado xs (y-x)

-- 2ª solución
-- ===========

mayorPrefijoAcotado2 :: [Int] -> Int -> [Int]
mayorPrefijoAcotado2 xs y =
  take (longitudMayorPrefijoAcotado2 xs y) xs

longitudMayorPrefijoAcotado2 :: [Int] -> Int -> Int
longitudMayorPrefijoAcotado2 xs y =
  length (takeWhile (<=y) (map sum (inits xs))) - 1

-- 3ª solución
-- ===========

mayorPrefijoAcotado3 :: [Int] -> Int -> [Int]
mayorPrefijoAcotado3 xs y =
  take (longitudMayorPrefijoAcotado3 xs y) xs

longitudMayorPrefijoAcotado3 :: [Int] -> Int -> Int
longitudMayorPrefijoAcotado3 xs y =
  length (takeWhile (<= y) (scanl1 (+) xs))

-- Equivalencia
-- ============

-- La propiedad es
prop_equiv :: [Int] -> Int -> Bool
prop_equiv xs y =
  mayorPrefijoAcotado xs' y' == mayorPrefijoAcotado2 xs' y' &&
  mayorPrefijoAcotado xs' y' == mayorPrefijoAcotado3 xs' y' 
  where xs' = map abs xs
        y'  = abs y

-- La comprobación es
--    λ> quickCheck prop_equiv
--    +++ OK, passed 100 tests.
--    (0.01 secs, 2,463,688 bytes)

-- Comparación de eficiencia
-- =========================

--    λ> length (mayorPrefijoAcotado (repeat 1) (2*10^4))
--    20000
--    (0.04 secs, 5,086,544 bytes)
--    λ> length (mayorPrefijoAcotado2 (repeat 1) (2*10^4))
--    20000
--    (11.22 secs, 27,083,980,168 bytes)
--    λ> length (mayorPrefijoAcotado3 (repeat 1) (2*10^4))
--    20000
--    (0.02 secs, 4,768,992 bytes)
--    
--    λ> length (mayorPrefijoAcotado (repeat 1) (8*10^6))
--    8000000
--    (3.19 secs, 1,984,129,832 bytes)
--    λ> length (mayorPrefijoAcotado3 (repeat 1) (8*10^6))
--    8000000
--    (1.02 secs, 1,856,130,936 bytes)
\end{code} 
