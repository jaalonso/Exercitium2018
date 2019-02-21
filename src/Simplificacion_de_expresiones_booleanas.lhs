% Simplificacion_de_expresiones_booleanas.lhs
% Simplificación de expresiones booleanas.
% José A. Alonso Jiménez
% Sevilla, 14 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{¿Dices que nada se pierde? \\
     Si esta copa de cristal \\
     se me rompe, nunca en ella \\
     beberé, nunca jamás.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Simplificacion_de_expresiones_booleanas where
\end{code}
\end{comment}

El siguiente tipo de dato algebraico representa las expresiones
booleanas construidas con una variable (X), las constantes
verdadera (V) y falsa (F), la negación (Neg) y la disyunción (Dis):
\begin{descripcion} 
  data Expr = X
            | V
            | F
            | Neg Expr
            | Dis Expr Expr
            deriving (Eq, Ord)
\end{descripcion}             
Por ejemplo, la fórmula \(\neg X \vee V\) se representa por (Dis (Neg X) V).

Definir las funciones
\begin{descripcion} 
  valor      :: Expr -> Bool -> Bool 
  simplifica :: Expr -> Expr
\end{descripcion}   
tales que (valor p i) es el valor de la fórmula p cuando se le asigna
a X el valor i. Por ejemplo,
\begin{descripcion} 
  valor (Neg X) True           ==  False
  valor (Neg F) True           ==  True
  valor (Dis X (Neg X)) True   ==  True
  valor (Dis X (Neg X)) False  ==  True
\end{descripcion}   
y (simplifica p) es una expresión obtenida aplicándole a p las siguientes
reglas de simplificación:
\begin{descripcion} 
  Neg V       = F
  Neg F       = V
  Neg (Neg q) = q
  Dis V q     = V
  Dis F q     = q
  Dis q V     = V
  Dis q F     = F
  Dis q q     = q
\end{descripcion}   
Por ejemplo,
\begin{descripcion} 
  simplifica (Dis X (Neg (Neg X)))                      ==  X
  simplifica (Neg (Dis (Neg (Neg X)) F))                ==  Neg X
  simplifica (Dis (Neg F) F)                            ==  V
  simplifica (Dis (Neg V) (Neg (Dis (Neg X) F)))        ==  X
  simplifica (Dis (Neg V) (Neg (Dis (Neg (Neg X)) F)))  ==  Neg X
\end{descripcion}

Comprobar con QuickCheck que para cualquier fórmula p y cualquier
booleano i se verifica que (valor (simplifica p) i) es igual a 
(valor p i).

Para la comprobación, de define el generador
\begin{descripcion} 
   instance Arbitrary Expr where
     arbitrary = sized expr
       where expr n | n <= 0    = atom
                    | otherwise = oneof [ atom
                                        , liftM Neg subexpr
                                        , liftM2 Dis subexpr subexpr ]
               where atom    = oneof [elements [X,V,F]]
                     subexpr = expr (n `div` 2)
\end{descripcion} 
que usa las funciones liftM y liftM2 de la librería Control.Monad que
hay que importar al principio.

\section*{Soluciones}

\begin{code} 
import Test.QuickCheck
import Control.Monad 

data Expr = X
          | V
          | F
          | Neg Expr
          | Dis Expr Expr
          deriving (Eq, Ord, Show)

valor :: Expr -> Bool -> Bool 
valor X i         = i
valor V _         = True
valor F _         = False
valor (Neg p) i   = not (valor p i)
valor (Dis p q) i = valor p i || valor q i

simplifica :: Expr -> Expr
simplifica X = X
simplifica V = V
simplifica F = F
simplifica (Neg p) = negacion (simplifica p)
  where negacion V        = F
        negacion F        = V
        negacion (Neg p') = p'
        negacion p'       = Neg p'
simplifica (Dis p q) = disyuncion (simplifica p) (simplifica q)
  where disyuncion V _  = V
        disyuncion F q' = q'
        disyuncion _ V  = V
        disyuncion q' F = q'
        disyuncion p' q' | p' == q'  = p'
                         | otherwise = Dis p' q'

-- La propiedad es
prop_simplifica :: Expr -> Bool -> Bool
prop_simplifica p i =
  valor (simplifica p) i == valor p i

-- La comprobación es
--    λ> quickCheck prop_simplifica
--    +++ OK, passed 100 tests.

-- Generador de fórmulas
instance Arbitrary Expr where
  arbitrary = sized expr
    where expr n | n <= 0    = atom
                 | otherwise = oneof [ atom
                                     , liftM Neg subexpr
                                     , liftM2 Dis subexpr subexpr ]
            where atom    = oneof [elements [X,V,F]]
                  subexpr = expr (n `div` 2)
\end{code} 
