%  Ultimo_digito_no_nulo_del_factorial.hs
%  Último dígito no nulo del factorial
%  José A. Alonso Jiménez
%  Sevilla, 13 de noviembre de 2018
%  ---------------------------------------------------------------------

\epigraph {\textit{Incierto es, lo porvenir. ¿Quién sabe lo que va a
    pasar? Pero incierto es también lo pretérito. ¿Quién sabe lo que ha
    pasado? De suerte que ni el porvenir está escrito en ninguna parte,
    ni el pasado tampoco.}} {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Ultimo_digito_no_nulo_del_factorial where
\end{code}
\end{comment}

El factorial de 7 es $7! = 1 * 2 * 3 * 4 * 5 * 6 * 7 = 5040$. 
Por tanto, el último dígito no nulo del factorial de 7 es 4.

Definir la función
\begin{descripcion}
  ultimoNoNuloFactorial :: Integer -> Integer
\end{descripcion}
tal que (ultimoNoNuloFactorial n) es el último dígito no nulo del
factorial de n. Por ejemplo,
\begin{descripcion}
  ultimoNoNuloFactorial  7  == 4
  ultimoNoNuloFactorial 10  == 8
  ultimoNoNuloFactorial 12  == 6
  ultimoNoNuloFactorial 97  == 2
  ultimoNoNuloFactorial  0  == 1
\end{descripcion}

Comprobar con QuickCheck que si n es mayor que 4, entonces el último
dígito no nulo del factorial de n es par.

\section*{Solución}

\begin{code}
import Test.QuickCheck

-- 1ª definición
-- =============

ultimoNoNuloFactorial :: Integer -> Integer
ultimoNoNuloFactorial n = ultimoNoNulo (factorial n)

-- (ultimoNoNulo n) es el último dígito no nulo de n. Por ejemplo,
--    ultimoNoNulo 5040  ==  4
ultimoNoNulo :: Integer -> Integer
ultimoNoNulo n
  | m /= 0    = m
  | otherwise = ultimoNoNulo (n `div` 10)
  where m = n `rem` 10
        
-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 7  ==  5040
factorial :: Integer -> Integer
factorial n = product [1..n]

-- 2ª definición
-- =============

ultimoNoNuloFactorial2 :: Integer -> Integer
ultimoNoNuloFactorial2 n = ultimoNoNulo2 (factorial n)

-- (ultimoNoNulo2 n) es el último dígito no nulo de n. Por ejemplo,
--    ultimoNoNulo 5040  ==  4
ultimoNoNulo2 :: Integer -> Integer
ultimoNoNulo2 n = read [head (dropWhile (=='0') (reverse (show n)))]

-- Comprobación
-- ============

-- La propiedad es
prop_ultimoNoNuloFactorial :: Integer -> Property
prop_ultimoNoNuloFactorial n = 
  n > 4 ==> even (ultimoNoNuloFactorial n)
                  
-- La comprobación es
--    ghci> quickCheck prop_ultimoNoNuloFactorial
--    +++ OK, passed 100 tests.
\end{code}
