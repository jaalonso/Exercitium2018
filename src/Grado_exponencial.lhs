% Grado_exponencial.lhs
% Grado exponencial.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 19 de Diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Grado_exponencial where 
\end{code}
\end{comment}

El grado exponencial de un número n es el menor número e mayor que 1
tal que n es una subcadena de \(n^e\). Por ejemplo, el grado exponencial
de 2 es 5 ya que 2 es una subcadena de 32 (que es \(2^5\)) y nos es
subcadena de las anteriores potencias de 2 (2, 4 y 16). El grado
exponencial de 25 es 2 porque 25 es una subcadena de 625 (que es
\(25^2\)). 

Definir la función
\begin{descripcion} 
   gradoExponencial :: Integer -> Integer
\end{descripcion} 
tal que (gradoExponencial n) es el grado exponencial de n. Por
ejemplo,
\begin{descripcion} 
   gradoExponencial 2      ==  5
   gradoExponencial 25     ==  2
   gradoExponencial 15     ==  26
   gradoExponencial 1093   ==  100
   gradoExponencial 10422  ==  200
   gradoExponencial 11092  ==  300
\end{descripcion} 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck
import Data.List (genericLength, isInfixOf)

-- 1ª solución
-- ===========

gradoExponencial :: Integer -> Integer
gradoExponencial n =
  head [e | e <- [2..]
          , show n `isInfixOf` show (n^e)]

-- 2ª solución
-- ===========

gradoExponencial2 :: Integer -> Integer
gradoExponencial2 n =
  2 + genericLength (takeWhile noSubcadena (potencias n))
  where c             = show n
        noSubcadena x = not (c `isInfixOf`show x)

-- (potencias n) es la lista de las potencias de n a partir de n^2. Por
-- ejemplo, 
--    λ> take 10 (potencias 2)
--    [4,8,16,32,64,128,256,512,1024,2048]
potencias :: Integer -> [Integer]
potencias n =
  iterate (*n) (n^2)

-- 3ª solución
-- ===========

gradoExponencial3 :: Integer -> Integer
gradoExponencial3 n = aux 2
  where aux x
          | cs `isInfixOf` show (n^x) = x
          | otherwise                 = aux (x+1)
        cs = show n

-- Equivalencia
-- ============

-- La propiedad es
prop_gradosExponencial_equiv :: (Positive Integer) -> Bool
prop_gradosExponencial_equiv (Positive n) =
  gradoExponencial n == gradoExponencial2 n &&
  gradoExponencial n == gradoExponencial3 n

-- La comprobación es
--    λ> quickCheck prop_gradosExponencial_equiv
--    +++ OK, passed 100 tests.
\end{code} 

\section*{Referencia}

Basado en la \href{https://oeis.org/A045537}{sucesión A045537 de la OEIS}.
