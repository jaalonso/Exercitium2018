% Elemento_del_arbol_binario_completo_segun_su_posicion.hs
% Elemento del árbol binario completo según su posición.
% José A. Alonso Jiménez 
% Sevilla, 6 de diciembre de 2018
% ---------------------------------------------------------------------

\begin{comment}
\begin{code}
module Elemento_del_arbol_binario_completo_segun_su_posicion where    
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
  elementoEnPosicion :: Posicion -> Integer
\end{descripcion}
tal que (elementoEnPosicion ms) es el elemento en la posición ms. Por
ejemplo,
\begin{descripcion}
  elementoEnPosicion [D,I]    ==  6
  elementoEnPosicion [D,D]    ==  7
  elementoEnPosicion [I,I,D]  ==  9
  elementoEnPosicion []       ==  1
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

elementoEnPosicion :: Posicion -> Integer
elementoEnPosicion ms =
  aux ms (arbolBinarioCompleto (2^(1 + length ms)))
  where aux []     (N x _ _)  = x
        aux (I:ms') (N _ i _) = aux ms' i
        aux (D:ms') (N _ _ d) = aux ms' d
        aux _      _          = error "Imposible"

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

-- 2ª solución
-- ===========

elementoEnPosicion2 :: Posicion -> Integer
elementoEnPosicion2 = aux . reverse
  where aux []     = 1
        aux (I:ms) = 2 * aux ms
        aux (D:ms) = 2 * aux ms + 1

-- Equivalencia
-- ============

-- La propiedad es
prop_elementoEnPosicion_equiv :: Positive Integer -> Bool
prop_elementoEnPosicion_equiv (Positive n) =
  elementoEnPosicion  ps == n &&
  elementoEnPosicion2 ps == n 
  where ps = posicionDeElemento n

-- (posicionDeElemento n) es la posición del elemento n en el
-- árbol binario completo. Por ejemplo,
--    posicionDeElemento 6  ==  [D,I]
--    posicionDeElemento 7  ==  [D,D]
--    posicionDeElemento 9  ==  [I,I,D]
--    posicionDeElemento 1  ==  []
posicionDeElemento :: Integer -> Posicion
posicionDeElemento n =
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

-- La comprobación es
--    λ> quickCheck prop_elementoEnPosicion_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    λ> length (show (elementoEnPosicion (replicate (3*10^5) D)))
--    90310
--    (1.96 secs, 11,518,771,016 bytes)
--    λ> length (show (elementoEnPosicion2 (replicate (3*10^5) D)))
--    90310
--    (14.32 secs, 11,508,181,176 bytes)
\end{code}
