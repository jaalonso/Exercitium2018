% Numeros_primos_sumas_de_dos_primos.hs
% Números primos sumas de dos primos.
% José A. Alonso Jiménez
% Sevilla, 26 de Noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Numeros_primos_sumas_de_dos_primos where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 26--11--18}

Definir las funciones
\begin{descripcion}
  esPrimoSumaDeDosPrimos :: Integer -> Bool
  primosSumaDeDosPrimos :: [Integer]
\end{descripcion}
tales que
\begin{itemize}
\item 
\item (esPrimoSumaDeDosPrimos x) se verifica si x es un número primo que
  se puede escribir como la suma de dos números primos. Por ejemplo,
\begin{descripcion}  
  esPrimoSumaDeDosPrimos 19  ==  True
  esPrimoSumaDeDosPrimos 20  ==  False
  esPrimoSumaDeDosPrimos 23  ==  False
\end{descripcion}
\item primosSumaDeDosPrimos es la lista de los números primos que se
  pueden escribir como la suma de dos números primos. Por ejemplo,
\begin{descripcion}  
  λ> take 17 primosSumaDeDosPrimos
  [5,7,13,19,31,43,61,73,103,109,139,151,181,193,199,229,241]
  λ> primosSumaDeDosPrimos !! (10^5)
  18409543
\end{descripcion}     
\end{itemize}

\section*{Soluciones}

\begin{code}
import Data.Numbers.Primes (isPrime, primes)
import Test.QuickCheck

-- 1ª solución
-- ===========

esPrimoSumaDeDosPrimos :: Integer -> Bool
esPrimoSumaDeDosPrimos x =
  isPrime x && isPrime (x - 2)

primosSumaDeDosPrimos :: [Integer]
primosSumaDeDosPrimos =
  [x | x <- primes
     , isPrime (x - 2)]

-- 2ª solución
-- ===========

primosSumaDeDosPrimos2 :: [Integer]
primosSumaDeDosPrimos2 =
  [y | (x,y) <- zip primes (tail primes)
     , y == x + 2]

esPrimoSumaDeDosPrimos2 :: Integer -> Bool
esPrimoSumaDeDosPrimos2 x = 
  x == head (dropWhile (<x) primosSumaDeDosPrimos2)

-- Equivalencias
-- =============

-- Equivalencia de esPrimoSumaDeDosPrimos
prop_esPrimoSumaDeDosPrimos_equiv :: Integer -> Property
prop_esPrimoSumaDeDosPrimos_equiv x =
  x > 0 ==>
  esPrimoSumaDeDosPrimos x == esPrimoSumaDeDosPrimos2 x

-- La comprobación es
--    λ> quickCheck prop_esPrimoSumaDeDosPrimos_equiv
--    +++ OK, passed 100 tests.

-- Equivalencia de primosSumaDeDosPrimos
prop_primosSumaDeDosPrimos_equiv :: Int -> Property
prop_primosSumaDeDosPrimos_equiv n =
  n >= 0 ==>
  primosSumaDeDosPrimos !! n == primosSumaDeDosPrimos2 !! n

-- La comprobación es
--    λ> quickCheck prop_primosSumaDeDosPrimos_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> primosSumaDeDosPrimos !! (10^4)
--    1261081
--    (2.07 secs, 4,540,085,256 bytes)
--
-- Se recarga para evitar memorización    
--    λ> primosSumaDeDosPrimos2 !! (10^4)
--    1261081
--    (0.49 secs, 910,718,408 bytes)
\end{code}
