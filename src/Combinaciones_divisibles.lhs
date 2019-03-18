% Combinaciones_divisibles.lhs
% Combinaciones divisibles.
% José A. Alonso Jiménez
% Sevilla, 11 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{El que espera desespera, \\
     dice la voz popular. \\
     ¡Qué verdad tan verdadera! \\
     La verdad es lo que es, \\
     y sigue siendo verdad \\
     aunque se piense al revés.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Combinaciones_divisibles where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  tieneCombinacionDivisible :: [Int] -> Int -> Bool
\end{descripcion}   
tal que (tieneCombinacionDivisible xs m) se verifica si existe alguna
forma de combinar todos los elementos de la lista (con las
operaciones suma o resta) de forma que el resultado sea divisible por
m. Por ejemplo,
\begin{descripcion} 
  tieneCombinacionDivisible [1,3,4,6] 4  ==  True
  tieneCombinacionDivisible [1,3,9]   2  ==  False
\end{descripcion}   
En el primer ejemplo, 1 - 2 + 3 + 4 + 6 = 12 es una combinación
divisible por 4. En el segundo ejemplo, las combinaciones de [1,3,9]
son
\begin{descripcion} 
  1 + 3 + 9 =  13
 -1 + 3 + 9 =  11
  1 - 3 + 9 =   7
 -1 - 3 + 9 =   5
  1 + 3 - 9 =  -5
 -1 + 3 - 9 =  -7
  1 - 3 - 9 = -11
 -1 - 3 - 9 = -13
\end{descripcion}   
y ninguna de las 4 es divisible por 2.

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- 1ª solución
-- ===========

tieneCombinacionDivisible :: [Int] -> Int -> Bool
tieneCombinacionDivisible xs m =
  any esDivisible (valoresCombinaciones xs)
  where esDivisible x = x `mod` m == 0

-- (valoresCombinaciones xs) es la lista de los valores de todas las
-- combinaciones de todos los elementos de la lista con las operaciones
-- suma o resta. Por ejemplo,
--    λ> valoresCombinaciones [1,3,4,6]
--    [14,12,8,6,6,4,0,-2,2,0,-4,-6,-6,-8,-12,-14]
--    λ> valoresCombinaciones [1,3,-4,6]
--    [6,4,0,-2,14,12,8,6,-6,-8,-12,-14,2,0,-4,-6]
valoresCombinaciones :: [Int] -> [Int]
valoresCombinaciones []     = []
valoresCombinaciones [x]    = [x,-x]
valoresCombinaciones (x:xs) = concat [[y + x, y - x] | y <- ys]
  where ys = valoresCombinaciones xs

-- 2ª solución
-- ===========

tieneCombinacionDivisible2 :: [Int] -> Int -> Bool
tieneCombinacionDivisible2 xs m =
  tieneCombinacionCongruente xs m 0

-- (tieneCombinacionCongruente xs m a) se verifica si existe alguna
-- forma de combinar todos los elementos de la lista xs (con las
-- operaciones suma o resta) de forma que el resultado sea congruente
-- con a módulo m. Por ejemplo,
--    tieneCombinacionCongruente [1,3,4,6] 4 0  ==  True
--    tieneCombinacionCongruente [1,3,4,6] 4 1  ==  False
--    tieneCombinacionCongruente [1,3,9] 2 0    ==  False
--    tieneCombinacionCongruente [1,3,9] 2 1    ==  True
tieneCombinacionCongruente :: [Int] -> Int -> Int -> Bool
tieneCombinacionCongruente []  _  _ = False
tieneCombinacionCongruente [x] m  a = (x - a) `mod` m == 0
tieneCombinacionCongruente (x:xs) m a =
  tieneCombinacionCongruente xs m (a-x) ||
  tieneCombinacionCongruente xs m (a+x)

-- Equivalencia
-- ============

-- La propiedad es
prop_tieneCombinacionDivisible :: [Int] -> Positive Int -> Bool
prop_tieneCombinacionDivisible xs (Positive m) =
  tieneCombinacionDivisible xs m == tieneCombinacionDivisible2 xs m

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=25}) prop_tieneCombinacionDivisible
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> (n,xs,m) = (200,[-n..n],sum [1..n]) 
--    (0.00 secs, 0 bytes)
--    λ> and [tieneCombinacionDivisible xs a | a <- [1..m]]
--    True
--    (4.74 secs, 6,536,494,976 bytes)
--    λ> and [tieneCombinacionDivisible2 xs a | a <- [1..m]]
--    True
--    (2.97 secs, 3,381,932,664 bytes)
\end{code} 
