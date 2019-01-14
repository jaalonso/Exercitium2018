% Cadena_descendiente_de_subnumeros.lhs
% Cadena descendiente de subnúmeros.
% José A. Alonso Jiménez
% Sevilla, 7 de enero de 2019
% ---------------------------------------------------------------------

\epigraph {\textit{La inseguridad, la incertidumbre, la desconfianza,
    son acaso nuestras únicas verdades. Hay que aferrarse a ellas.}}
    {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Cadena_descendiente_de_subnumeros where
\end{code}
\end{comment}

Una particularidad del 2019 es que se puede escribir como una
cadena de dos subnúmeros consecutivos (el 20 y el 19).

Definir la función
\begin{descripcion} 
  cadena :: Integer -> [Integer]
\end{descripcion} 
tal que (cadena n) es la cadena de subnúmeros consecutivos de n cuya
unión es n; es decir, es la lista de números [x,x-1,...x-k] tal que
su concatenación es n. Por ejemplo,
\begin{descripcion} 
  cadena 2019         == [20,19]
  cadena 2018         == [2018]
  cadena 1009         == [1009]
  cadena 110109       == [110,109]
  cadena 201200199198 == [201,200,199,198] 
  cadena 3246         == [3246]            
  cadena 87654        == [8,7,6,5,4]       
  cadena 123456       == [123456]          
  cadena 1009998      == [100,99,98]       
  cadena 100908       == [100908]          
  cadena 1110987      == [11,10,9,8,7]     
  cadena 210          == [2,1,0]           
  cadena 1            == [1]               
  cadena 0            == [0]               
  cadena 312          == [312]             
  cadena 191          == [191]
  length (cadena (read (concatMap show [2019,2018..0])))  ==  2020
\end{descripcion}

\textbf{Nota}: Los subnúmeros no pueden empezar por cero. Por ejemplo, [10,09]
no es una cadena de 1009 como se observa en el tercer ejemplo.

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck
import Data.List (inits)

-- 1ª solución
-- ===========

cadena :: Integer -> [Integer]
cadena = head . cadenasL . digitos

-- (digitos n) es la lista de los dígitos de n. Por ejemplo, 
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- (cadenasL xs) son las cadenas descendientes del número cuyos dígitos
-- son xs. Por ejemplo,
--    cadenasL [2,0,1,9]      == [[20,19],[2019]]
--    cadenasL [1,0,0,9]      == [[1009]]
--    cadenasL [1,1,0,1,0,9]  == [[110,109],[110109]]
cadenasL :: [Integer] -> [[Integer]] 
cadenasL []       = []
cadenasL [x]      = [[x]]
cadenasL [1,0]    = [[1,0],[10]]
cadenasL (x:0:zs) = cadenasL (10*x:zs) 
cadenasL (x:y:zs) =
     [x:a:as | (a:as) <- cadenasL (y:zs), a == x-1]
  ++ cadenasL (10*x+y:zs)

-- 2ª solución
-- ===========

cadena2 :: Integer -> [Integer]
cadena2 n = (head . concatMap aux . iniciales) n 
  where aux x = [[x,x-1..x-k] | k <- [0..x]
                              , concatMap show [x,x-1..x-k] == ds]
        ds    = show n

-- (iniciales n) es la lista de los subnúmeros iniciales de n. Por
-- ejemplo, 
--    iniciales 2019  ==  [2,20,201,2019]
iniciales :: Integer -> [Integer]
iniciales = map read . tail . inits . show

-- Equivalencia
-- ============

-- La propiedad es
prop_cadena :: Positive Integer -> Bool
prop_cadena (Positive n) =
  cadena n == cadena2 n 
  
-- La comprobación es
--    λ> quickCheck prop_cadena
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> length (cadena (read (concatMap show [15,14..0])))
--    16
--    (3.28 secs, 452,846,008 bytes)
--    λ> length (cadena2 (read (concatMap show [15,14..0])))
--    16
--    (0.03 secs, 176,360 bytes)
\end{code} 
