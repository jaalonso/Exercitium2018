% Digitos_pares_de_cuadrados.lhs
% Dígitos en las posiciones pares de cuadrados.
% José A. Alonso Jiménez
% Sevilla, 1 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¡Ojos que a la luz se abrieron \\
     un día para, después, \\
     ciegos tornar a la tierra,\\
     hartos de mirar sin ver.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Digitos_pares_de_cuadrados where
\end{code}
\end{comment}

Definir las funciones
\begin{descripcion} 
  digitosPosParesCuadrado    :: Integer -> ([Integer],Int)
  invDigitosPosParesCuadrado :: ([Integer],Int) -> [Integer]
\end{descripcion}   
tales que
\begin{itemize}
\item (digitosPosParesCuadrado n) es el par formados por los dígitos de
  n² en la posiciones pares y por el número de dígitos de n². Por
  ejemplo,
\begin{descripcion}   
  digitosPosParesCuadrado 8     ==  ([6],2)
  digitosPosParesCuadrado 14    ==  ([1,6],3)
  digitosPosParesCuadrado 36    ==  ([1,9],4)
  digitosPosParesCuadrado 116   ==  ([1,4,6],5)
  digitosPosParesCuadrado 2019  ==  ([4,7,3,1],7)
\end{descripcion}  
\item (invDigitosPosParesCuadrado (xs,k)) es la lista de los números n
  tales que xs es la lista de los dígitos de n² en la posiciones
  pares y k es el número de dígitos de n². Por ejemplo,
\begin{descripcion}   
  invDigitosPosParesCuadrado ([6],2)             ==  [8]
  invDigitosPosParesCuadrado ([1,6],3)           ==  [14]
  invDigitosPosParesCuadrado ([1,9],4)           ==  [36]
  invDigitosPosParesCuadrado ([1,4,6],5)         ==  [116,136]
  invDigitosPosParesCuadrado ([4,7,3,1],7)       ==  [2019,2139,2231]
  invDigitosPosParesCuadrado ([1,2],3)           ==  []
  invDigitosPosParesCuadrado ([1,2],4)           ==  [32,35,39]
  invDigitosPosParesCuadrado ([1,2,3,4,5,6],11)  ==  [115256,127334,135254]
\end{descripcion} 
\end{itemize}

Comprobar con QuickCheck que para todo entero positivo n se verifica
que para todo entero positivo m, m pertenece a
(invDigitosPosParesCuadrado (digitosPosParesCuadrado n)) si, y sólo si,
(digitosPosParesCuadrado m) es igual a (digitosPosParesCuadrado n)

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

-- Definición de digitosPosParesCuadrado
-- =====================================

digitosPosParesCuadrado :: Integer -> ([Integer],Int)
digitosPosParesCuadrado n =
  (digitosPosPares (n^2),length (show (n^2)))

-- (digitosPosPares n) es la lista de los dígitos de n en posiciones
-- pares. Por ejemplo,
--    digitosPosPares 24012019  ==  [2,0,2,1]
digitosPosPares :: Integer -> [Integer]
digitosPosPares n = elementosPosPares (digitos n)

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- (elementosPosPares xs) es la lista de los elementos de xs en
-- posiciones pares. Por ejemplo,
--    elementosPosPares [3,2,5,7,6,4]  ==  [3,5,6]
elementosPosPares :: [a] -> [a]
elementosPosPares []       = []
elementosPosPares [x]      = [x]
elementosPosPares (x:_:zs) = x : elementosPosPares zs

-- 1ª definición de invDigitosPosParesCuadrado
-- ========================================

invDigitosPosParesCuadrado :: ([Integer],Int) -> [Integer]
invDigitosPosParesCuadrado (xs, a) =
  [x | x <- [ceiling (sqrt 10^(a-1))..ceiling (sqrt 10^a)]
     , digitosPosParesCuadrado x == (xs,a)]

-- 2ª definición de invDigitosPosParesCuadrado
-- ========================================

invDigitosPosParesCuadrado2 :: ([Integer],Int) -> [Integer]
invDigitosPosParesCuadrado2 x =
  [n | n <- [a..b], digitosPosParesCuadrado n == x]
  where a = floor (sqrt (fromIntegral (completaNum x 0)))
        b = ceiling (sqrt (fromIntegral (completaNum x 9)))

-- (completaNum (xs,k) n) es el número cuyos dígitos en las posiciones
-- pares son los de xs y los de las posiciones impares son iguales a n
-- (se supone que k es igual al doble de la longitud de xs o un
-- menos). Por ejemplo, 
--    completaNum ([1,3,8],5) 4  ==  14348
--    completaNum ([1,3,8],6) 4  ==  143484
completaNum :: ([Integer],Int) -> Integer -> Integer
completaNum x n = digitosAnumero (completa x n)

-- (completa (xs,k) n) es la lista cuyos elementos en las posiciones
-- pares son los de xs y los de las posiciones impares son iguales a n
-- (se supone que k es igual al doble de la longitud de xs o un
-- menos). Por ejemplo, 
--    completa ([1,3,8],5) 4  ==  [1,4,3,4,8]
--    completa ([1,3,8],6) 4  ==  [1,4,3,4,8,4]
completa :: ([Integer],Int) -> Integer -> [Integer]
completa (xs,k) n
  | even k    = ys
  | otherwise = init ys
  where ys = concat [[x,n] | x <- xs]

-- (digitosAnumero ds) es el número cuyos dígitos son ds. Por ejemplo,
--    digitosAnumero [2,0,1,9]  ==  2019
digitosAnumero :: [Integer] -> Integer
digitosAnumero = read . concatMap show

-- Comparación de eficiencia
-- =========================

--    λ> invDigitosPosParesCuadrado ([1,2,1,5,7,4,9],13)
--    [1106393,1234567,1314597]
--    (7.55 secs, 13,764,850,536 bytes)
--    λ> invDigitosPosParesCuadrado2 ([1,2,1,5,7,4,9],13)
--    [1106393,1234567,1314597]
--    (1.96 secs, 3,780,368,816 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es  
prop_digitosPosParesCuadrado :: Positive Integer -> Positive Integer -> Bool
prop_digitosPosParesCuadrado (Positive n) (Positive m) =
  (digitosPosParesCuadrado m == x)
  == (m `elem` invDigitosPosParesCuadrado x)
  where x = digitosPosParesCuadrado n

-- La comprobación es
--    λ> quickCheck prop_digitosPosParesCuadrado
--    +++ OK, passed 100 tests.
\end{code} 
