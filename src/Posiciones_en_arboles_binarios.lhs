%  Posiciones_en_arboles_binarios.hs
%  Posiciones en árboles binarios.
%  José A. Alonso Jiménez 
%  Sevilla, 2 de diciembre de 2018
%  ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Posiciones_en_arboles_binarios where    
\end{code}
\end{comment}

\section*{Enunciado}

Los árboles binarios con datos en los nodos se definen por
\begin{descripcion}
   data Arbol a = H
                | N a (Arbol a) (Arbol a)
     deriving (Eq, Show)
\end{descripcion}
Por ejemplo, el árbol
\begin{descripcion}
          3
         / \
        /   \
       0     5
      / \   / \
     5   0 0   3
    / \
   2   4   
\end{descripcion}
se representa por
\begin{descripcion}
   ejArbol :: Arbol Int
   ejArbol = N 3
               (N 0
                  (N 5
                     (N 2 H H)
                     (N 4 H H))
                  (N 0 H H))
               (N 5
                  (N 0 H H)
                  (N 3 H H))
\end{descripcion}
                
Cada posición de un elemento de un árbol es una lista de movimientos
hacia la izquierda o hacia la derecha. Por ejemplo, la posición de 4
en al árbol anterior es [I,I,D].

Los tipos de los movimientos y de las posiciones se definen por
\begin{descripcion}
   data Movimiento = I | D deriving (Show, Eq)
   type Posicion   = [Movimiento]
\end{descripcion}
   
Definir la función
\begin{descripcion}
   posiciones :: Eq b => b -> Arbol b -> [Posicion]
\end{solucion}
tal que (posiciones n a) es la lista de las posiciones del elemento n
en el árbol a. Por ejemplo,
\begin{descripcion}
   posiciones 0 ejArbol  ==  [[I],[I,D],[D,I]]
   posiciones 2 ejArbol  ==  [[I,I,I]]
   posiciones 3 ejArbol  ==  [[],[D,D]]
   posiciones 4 ejArbol  ==  [[I,I,D]]
   posiciones 5 ejArbol  ==  [[I,I],[D]]
   posiciones 1 ejArbol  ==  []
\end{descripcion}
 
\section*{Soluciones}

\begin{code}
import Data.List (nub)
import Test.QuickCheck
  
data Arbol a = H
             | N a (Arbol a) (Arbol a)
  deriving (Eq, Show)

ejArbol :: Arbol Int
ejArbol = N 3
            (N 0
               (N 5
                  (N 2 H H)
                  (N 4 H H))
               (N 0 H H))
            (N 5
               (N 0 H H)
               (N 3 H H))

data Movimiento = I | D deriving (Show, Eq, Ord)

type Posicion = [Movimiento]

-- 1ª solución
-- ===========

posiciones :: Eq b => b -> Arbol b -> [Posicion]
posiciones n a = aux n a [[]]
  where aux _ H _                       = []
        aux n' (N x i d) cs | x == n'   = cs ++
                                           [I:xs | xs <- aux n' i cs] ++
                                           [D:xs | xs <- aux n' d cs]
                            | otherwise = [I:xs | xs <- aux n' i cs] ++
                                           [D:xs | xs <- aux n' d cs]
                   
-- 2ª solución
-- ===========

posiciones2 :: Eq b => b -> Arbol b -> [Posicion]
posiciones2 n a = aux n a [[]]
  where aux _ H _                      = []
        aux n' (N x i d) cs | x == n'    = cs ++ ps
                            | otherwise = ps
          where ps = [I:xs | xs <- aux n' i cs] ++
                     [D:xs | xs <- aux n' d cs]

-- 3ª solución
-- ===========

posiciones3 :: Eq b => b -> Arbol b -> [Posicion]
posiciones3 n a = aux n a [[]]
  where aux _ H _                      = []
        aux n' (N x i d) cs | x == n'    = cs ++ ps
                            | otherwise = ps
          where ps = map (I:) (aux n' i cs) ++
                     map (D:) (aux n' d cs)

-- Equivalencia
-- ============

-- Generador de árboles
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized genArbol

genArbol :: (Arbitrary a, Integral a1) => a1 -> Gen (Arbol a)
genArbol 0         = return H 
genArbol n | n > 0 = N <$> arbitrary <*> subarbol <*> subarbol
  where subarbol = genArbol (div n 2)
genArbol _         = error "Imposible"

-- La propiedad es
prop_posiciones_equiv :: Arbol Int -> Bool
prop_posiciones_equiv a =
  and [posiciones n a == posiciones2 n a | n <- xs] &&
  and [posiciones n a == posiciones3 n a | n <- xs]  
  where xs = take 3 (elementos a)

-- (elementos a) son los elementos del árbol a. Por ejemplo,
--    elementos ejArbol  ==  [3,0,5,2,4]
elementos :: Eq b => Arbol b -> [b]
elementos H         = []
elementos (N x i d) = nub (x : elementos i ++ elementos d)

-- La comprobación es
--    λ> quickCheck prop_posiciones_equiv
--    +++ OK, passed 100 tests.
\end{code}
 
