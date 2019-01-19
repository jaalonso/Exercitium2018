% Entre_dos_conjuntos.lhs
% Entre dos conjuntos.
% José A. Alonso Jiménez
% Sevilla, 12 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph {\textit{Las razones no se transmiten, se engendran, por
    cooperación, en el diálogo.}} {Antonio Machado}

\begin{comment}
\begin{code}
module Entre_dos_conjuntos where    
\end{code}
\end{comment}

\section*{Enunciado}

Se dice que un x número se encuentra entre dos conjuntos xs e ys si x
es divisible por todos los elementos de xs y todos los elementos de
zs son divisibles por x. Por ejemplo, 12 se encuentra entre los
conjuntos {2, 6} y {24, 36}.

Definir la función
\begin{descripcion}
  entreDosConjuntos :: [Int] -> [Int] -> [Int]
\end{descripcion}
tal que (entreDosConjuntos xs ys) es la lista de elementos entre xs e
ys (se supone que xs e ys son listas no vacías de números enteros
positivos). Por ejemplo,
\begin{descripcion}
  entreDosConjuntos [2,6] [24,36]     ==  [6,12]
  entreDosConjuntos [2,4] [32,16,96]  ==  [4,8,16]
\end{descripcion}
Otros ejemplos
\begin{descripcion}
  λ> (xs,a) = ([1..15],product xs) 
  λ> length (entreDosConjuntos xs [a,2*a..10*a])
  270
  λ> (xs,a) = ([1..16],product xs) 
  λ> length (entreDosConjuntos xs [a,2*a..10*a])
  360
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Test.QuickCheck 

-- 1ª solución
-- ===========

entreDosConjuntos :: [Int] -> [Int] -> [Int]
entreDosConjuntos xs ys =
  [z | z <- [a..b]
     , and [z `mod` x == 0 | x <- xs]
     , and [y `mod` z == 0 | y <- ys]]
  where a = maximum xs
        b = minimum ys

-- 2ª solución
-- ===========

entreDosConjuntos2 :: [Int] -> [Int] -> [Int]
entreDosConjuntos2 xs ys =
  [z | z <- [a..b]
     , all (`divideA` z) xs
     , all (z `divideA`) ys]
  where a = mcmL xs
        b = mcdL ys

--    mcmL [2,3,18]  ==  18
--    mcmL [2,3,15]  ==  30
mcdL :: [Int] -> Int
mcdL [x]    = x
mcdL (x:xs) = gcd x (mcdL xs)

--    mcmL [12,30,18]  ==  6
--    mcmL [12,30,14]  ==  2
mcmL :: [Int] -> Int
mcmL [x]    = x
mcmL (x:xs) = lcm x (mcmL xs)

divideA :: Int -> Int -> Bool
divideA x y = y `mod` x == 0

-- 3ª solución
-- ===========

entreDosConjuntos3 :: [Int] -> [Int] -> [Int]
entreDosConjuntos3 xs ys =
  [z | z <- [a..b]
     , all (`divideA` z) xs
     , all (z `divideA`) ys]
  where a = mcmL2 xs
        b = mcdL2 ys

-- Definición equivalente
mcdL2 :: [Int] -> Int
mcdL2 = foldl1 gcd

-- Definición equivalente
mcmL2 :: [Int] -> Int
mcmL2 = foldl1 lcm

-- 4ª solución
-- ===========

entreDosConjuntos4 :: [Int] -> [Int] -> [Int]
entreDosConjuntos4 xs ys =
  [z | z <- [a,a+a..b]
     , z `divideA` b] 
  where a = mcmL2 xs
        b = mcdL2 ys

-- 5ª solución
-- ===========

entreDosConjuntos5 :: [Int] -> [Int] -> [Int]
entreDosConjuntos5 xs ys =
  filter (`divideA` b) [a,a+a..b]
  where a = mcmL2 xs
        b = mcdL2 ys

-- Equivalencia
-- ============

-- Para comprobar la equivalencia se define el tipo de listas no vacías
-- de números enteros positivos:
newtype ListaNoVaciaDePositivos = L [Int]
  deriving Show

-- genListaNoVaciaDePositivos es un generador de listas no vacióas de
-- enteros positivos. Por ejemplo,
--    λ> sample genListaNoVaciaDePositivos
--    L [1]
--    L [1,2,2]
--    L [4,3,4]
--    L [1,6,5,2,4]
--    L [2,8]
--    L [11]
--    L [13,2,3]
--    L [7,3,9,15,11,12,13,3,9,6,13,3]
--    L [16,2,11,10,6,5,16,4,1,15,9,11,8,15,2,15,7]
--    L [5,4,9,13,5,6,7]
--    L [7,4,6,12,2,11,6,14,14,13,14,11,6,2,18,8,16,2,13,9]
genListaNoVaciaDePositivos :: Gen ListaNoVaciaDePositivos
genListaNoVaciaDePositivos = do
  x  <- arbitrary
  xs <- arbitrary
  return (L (map ((+1) . abs) (x:xs)))

-- Generación arbitraria de listas no vacías de enteros positivos.
instance Arbitrary ListaNoVaciaDePositivos where
  arbitrary = genListaNoVaciaDePositivos

-- La propiedad es
prop_entreDosConjuntos_equiv ::
     ListaNoVaciaDePositivos
  -> ListaNoVaciaDePositivos
  -> Bool
prop_entreDosConjuntos_equiv (L xs) (L ys) =
  entreDosConjuntos xs ys == entreDosConjuntos2 xs ys &&
  entreDosConjuntos xs ys == entreDosConjuntos3 xs ys &&
  entreDosConjuntos xs ys == entreDosConjuntos4 xs ys &&
  entreDosConjuntos xs ys == entreDosConjuntos5 xs ys 

-- La comprobación es
--    λ> quickCheck prop_entreDosConjuntos_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> (xs,a) = ([1..10],product xs) 
--    λ> length (entreDosConjuntos xs [a,2*a..10*a])
--    36
--    (5.08 secs, 4,035,689,200 bytes)
--    λ> length (entreDosConjuntos2 xs [a,2*a..10*a])
--    36
--    (3.75 secs, 2,471,534,072 bytes)
--    λ> length (entreDosConjuntos3 xs [a,2*a..10*a])
--    36
--    (3.73 secs, 2,471,528,664 bytes)
--    λ> length (entreDosConjuntos4 xs [a,2*a..10*a])
--    36
--    (0.01 secs, 442,152 bytes)
--    λ> length (entreDosConjuntos5 xs [a,2*a..10*a])
--    36
--    (0.00 secs, 374,824 bytes)
\end{code}
