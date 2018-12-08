% Numeros_colinas.hs
% Números colinas.
% José A. Alonso Jiménez
% Sevilla, 25 de Noviembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Numeros_colinas where
\end{code}
\end{comment}

\section*{Ejercicio propuesto el 29--11--18}

Se dice que un número natural n es una colina si su primer dígito es
igual a su último dígito, los primeros dígitos son estrictamente
creciente hasta llegar al máximo, el máximo se puede repetir y los
dígitos desde el máximo al final son estrictamente decrecientes.

Definir la función
\begin{descripcion}
  esColina :: Integer -> Bool
\end{descripcion}
tal que (esColina n) se verifica si n es un número colina. Por
ejemplo,
\begin{descripcion}
  esColina 12377731  ==  True
  esColina 1237731   ==  True
  esColina 123731    ==  True
  esColina 12377730  ==  False
  esColina 12377730  ==  False
  esColina 10377731  ==  False
  esColina 12377701  ==  False
  esColina 33333333  ==  True
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Data.Char (digitToInt)
import Test.QuickCheck

-- 1ª definición
-- =============

esColina :: Integer -> Bool
esColina n =
  head ds == last ds &&
  esCreciente xs &&
  esDecreciente ys
  where ds = digitos n
        m  = maximum ds
        xs = takeWhile (<m) ds
        ys = dropWhile (==m) (dropWhile (<m) ds)

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 425  ==  [4,2,5]
digitos :: Integer -> [Int]
digitos n = map digitToInt (show n)

-- (esCreciente xs) se verifica si la lista xs es estrictamente
-- creciente. Por ejemplo,
--    esCreciente [2,4,7]  ==  True
--    esCreciente [2,2,7]  ==  False
--    esCreciente [2,1,7]  ==  False
esCreciente :: [Int] -> Bool
esCreciente xs = and [x < y | (x,y) <- zip xs (tail xs)]

-- (esDecreciente xs) se verifica si la lista xs es estrictamente
-- decreciente. Por ejemplo,
--    esDecreciente [7,4,2]  ==  True
--    esDecreciente [7,2,2]  ==  False
--    esDecreciente [7,1,2]  ==  False
esDecreciente :: [Int] -> Bool
esDecreciente xs = and [x > y | (x,y) <- zip xs (tail xs)]

-- 2ª definición
-- =============

esColina2 :: Integer -> Bool
esColina2 n =
  head ds == last ds &&
  null (dropWhile (==(-1)) (dropWhile (==0) (dropWhile (==1) xs)))
  where ds = digitos n
        xs = [signum (y-x) | (x,y) <- zip ds (tail ds)] 

-- Equivalencia
-- ============

-- La propiedad de equivalencia es
prop_esColina :: Integer -> Property
prop_esColina n =
  n >= 0 ==> esColina n == esColina2 n 

-- La comprobación es
--    λ> quickCheck prop_esColina
--    +++ OK, passed 100 tests.
\end{code}
