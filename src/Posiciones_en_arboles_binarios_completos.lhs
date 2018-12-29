% Posiciones_en_arboles_binarios_completos.hs
% Posiciones en árboles binarios completos.
% José A. Alonso Jiménez 
% Sevilla, 5 de diciembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Posiciones_en_arboles_binarios_completos where    
\end{code}
\end{comment}

\section*{Enunciado}

Un \href{http://bit.ly/2DUr53g}{árbol binario completo} es un árbol
binario que tiene todos los nodos posibles hasta el penúltimo nivel,
y donde los elementos del último nivel están colocados de izquierda a
derecha sin dejar huecos entre ellos.

La numeración de los árboles binarios completos se realiza a partir
de la raíz, recorriendo los niveles de izquierda a derecha. Por
ejemplo,
\begin{descripcion}   
                  1
                 /  \
                /    \
               /      \
              2        3
             / \      / \
            4   5    6   7
           / \    
          8   9 
\end{descripcion}

Los árboles binarios se puede representar mediante el siguiente tipo
\begin{descripcion}
   data Arbol = H
              | N Integer Arbol Arbol
     deriving (Show, Eq)
\end{descripcion}

Cada posición de un elemento de un árbol es una lista de movimientos
hacia la izquierda o hacia la derecha. Por ejemplo, la posición de 9
en al árbol anterior es [I,I,D].

Los tipos de los movimientos y de las posiciones se definen por
\begin{descripcion}
   data Movimiento = I | D deriving (Show, Eq)
   type Posicion   = [Movimiento]
\end{descripcion}

Definir la función
\begin{descripcion}
   posicionDeElemento :: Integer -> Posicion
\end{descripcion}
tal que (posicionDeElemento n) es la posición del elemento n en el
árbol binario completo. Por ejemplo,
\begin{descripcion}
   posicionDeElemento 6  ==  [D,I]
   posicionDeElemento 7  ==  [D,D]
   posicionDeElemento 9  ==  [I,I,D]
   posicionDeElemento 1  ==  []

   length (posicionDeElemento (10^50000))  ==  166096
\end{descripcion}

\section*{Soluciones}

\begin{code}
import Test.QuickCheck

data Arbol = H
           | N Integer Arbol Arbol
  deriving (Eq, Show)

data Movimiento = I | D deriving (Show, Eq)

type Posicion = [Movimiento]

-- 1ª solución
-- ===========

posicionDeElemento :: Integer -> Posicion
posicionDeElemento n =
  head (posiciones n (arbolBinarioCompleto n))

-- (arbolBinarioCompleto n) es el árbol binario completo con n
-- nodos. Por ejemplo, 
--    λ> arbolBinarioCompleto 4
--    N 1 (N 2 (N 4 H H) H) (N 3 H H)
--    λ> pPrint (arbolBinarioCompleto 9)
--    N 1
--      (N 2
--         (N 4
--            (N 8 H H)
--            (N 9 H H))
--         (N 5 H H))
--      (N 3
--         (N 6 H H)
--         (N 7 H H))
arbolBinarioCompleto :: Integer -> Arbol
arbolBinarioCompleto n = aux 1
  where aux i | i <= n    = N i (aux (2*i)) (aux (2*i+1))
              | otherwise = H

-- (posiciones n a) es la lista de las posiciones del elemento n
-- en el árbol a. Por ejemplo,
--    posiciones 9 (arbolBinarioCompleto 9)  ==  [[I,I,D]]
posiciones :: Integer -> Arbol -> [Posicion]
posiciones n a = aux n a [[]]
  where aux _ H _                      = []
        aux n' (N x i d) cs | x == n'    = cs ++ ps
                            | otherwise = ps
          where ps = map (I:) (aux n' i cs) ++
                     map (D:) (aux n' d cs)

-- 2ª solución
-- ===========

posicionDeElemento2 :: Integer -> Posicion
posicionDeElemento2 1 = []
posicionDeElemento2 n
  | even n    = posicionDeElemento2 (n `div` 2) ++ [I]
  | otherwise = posicionDeElemento2 (n `div` 2) ++ [D]

-- 3ª solución
-- ===========

posicionDeElemento3 :: Integer -> Posicion
posicionDeElemento3 = reverse . aux
  where aux 1 = []
        aux n | even n    = I : aux (n `div` 2) 
              | otherwise = D : aux (n `div` 2) 

-- 4ª solución
-- ===========

posicionDeElemento4 :: Integer -> Posicion
posicionDeElemento4 n =
  [f x | x <- tail (reverse (binario n))]
  where f 0 = I
        f 1 = D
        f _ = error "Imposible"

-- (binario n) es la lista de los dígitos de la representación binaria
-- de n. Por ejemplo,
--    binario 11  ==  [1,1,0,1]
binario :: Integer -> [Integer]
binario n
  | n < 2     = [n]
  | otherwise = n `mod` 2 : binario (n `div` 2)

-- Equivalencia
-- ============

-- La propiedad es
prop_posicionDeElemento_equiv :: Positive Integer -> Bool
prop_posicionDeElemento_equiv (Positive n) =
  posicionDeElemento n == posicionDeElemento2 n &&
  posicionDeElemento n == posicionDeElemento3 n &&
  posicionDeElemento n == posicionDeElemento4 n

-- La comprobación es
--    λ> quickCheck prop_posicionDeElemento_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> posicionDeElemento (10^7)
--    [I,I,D,D,I,I,I,D,I,I,D,I,D,D,I,D,I,I,I,I,I,I,I]
--    (5.72 secs, 3,274,535,328 bytes)
--    λ> posicionDeElemento2 (10^7)
--    [I,I,D,D,I,I,I,D,I,I,D,I,D,D,I,D,I,I,I,I,I,I,I]
--    (0.01 secs, 189,560 bytes)
--    λ> posicionDeElemento3 (10^7)
--    [I,I,D,D,I,I,I,D,I,I,D,I,D,D,I,D,I,I,I,I,I,I,I]
--    (0.01 secs, 180,728 bytes)
--    λ> posicionDeElemento4 (10^7)
--    [I,I,D,D,I,I,I,D,I,I,D,I,D,D,I,D,I,I,I,I,I,I,I]
--    (0.01 secs, 184,224 bytes)
--    
--    λ> length (posicionDeElemento2 (10^4000))
--    13287
--    (2.80 secs, 7,672,011,280 bytes)
--    λ> length (posicionDeElemento3 (10^4000))
--    13287
--    (0.03 secs, 19,828,744 bytes)
--    λ> length (posicionDeElemento4 (10^4000))
--    13287
--    (0.03 secs, 18,231,536 bytes)
--    
--    λ> length (posicionDeElemento3 (10^50000))
--    166096
--    (1.34 secs, 1,832,738,136 bytes)
--    λ> length (posicionDeElemento4 (10^50000))
--    166096
--    (1.70 secs, 1,812,806,080 bytes)
\end{code}
