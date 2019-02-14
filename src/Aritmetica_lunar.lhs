% Aritmetica_lunar.lhs
% Aritmética lunar.
% José A. Alonso Jiménez
% Sevilla, 7 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Cantad conmigo en coro: saber, nada sabemos, \\
     de arcano mar vinimos, a ignota mar iremos ... \\
     La luz nada ilumina y el sabio nada enseña. \\
     ¿Qué dice la palabra? ¿Qué el agua de la peña?}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Aritmetica_lunar where
\end{code}
\end{comment}

En la [aritmética lunar](http://bit.ly/2SaE9ZH) la suma y el producto
se hace como en la terrícola salvo que sus tablas de sumar y de
multiplicar son distintas. La suma lunar de dos dígitos es su máximo
(por ejemplo, 1 + 3 = 3 y 7 + 4 = 7) y el producto lunar de dos
dígitos es su mínimo (por ejemplo, 1 x 3 = 1 y 7 x 4 = 4). Por tanto,
\begin{descripcion} 
     3 5 7        3 5 7
   +   6 4      *   6 4
   -------      -------
     3 6 7        3 4 4
                3 5 6
                -------
                3 5 6 4
\end{descripcion}
              
Definir las funciones
\begin{descripcion} 
  suma     :: Integer -> Integer -> Integer
  producto :: Integer -> Integer -> Integer
\end{descripcion}   
tales que

+ (suma x y) es la suma lunar de x e y. Por ejemplo,
\begin{descripcion} 
  suma 357 64  ==  367
  suma 64 357  ==  367
  suma 1 3     ==  3
  suma 7 4     ==  7
\end{descripcion}   
+ (producto x y) es el producto lunar de x e y. Por ejemplo,
\begin{descripcion} 
  producto 357 64  ==  3564
  producto 64 357  ==  3564
  producto 1 3     ==  1
  producto 7 4     ==  4
\end{descripcion}

Comprobar con QuickCheck que la suma y el producto lunar son
conmutativos. 

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck

suma :: Integer -> Integer -> Integer
suma 0 0 = 0
suma x y = max x2 y2 + 10 * suma x1 y1
  where (x1,x2) = x `divMod` 10
        (y1,y2) = y `divMod` 10

producto :: Integer -> Integer -> Integer
producto 0 _ = 0
producto x y = productoDigitoNumero x2 y `suma` (10 * producto x1 y)
  where (x1, x2) = x `divMod` 10

-- (productoDigitoNumero d x) es el producto del dígito d por el número
-- x. Por ejemplo,
--    productoDigitoNumero 4 357  ==  344
--    productoDigitoNumero 6 357  ==  356
productoDigitoNumero :: Integer -> Integer -> Integer
productoDigitoNumero _ 0 = 0
productoDigitoNumero d x = min d x2 + 10 * productoDigitoNumero d x1
  where (x1, x2) = x `divMod` 10

-- La propiedad es
prop_conmutativa :: Positive Integer -> Positive Integer -> Bool
prop_conmutativa (Positive x) (Positive y) =
  suma x y == suma y x &&
  producto x y == producto y x

-- La comprobación es
--    λ> quickCheck prop_conmutativa
--    +++ OK, passed 100 tests.
\end{code} 