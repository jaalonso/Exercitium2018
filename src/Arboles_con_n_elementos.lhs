% Arboles_con_n_elementos.lhs
% Árboles con n elementos.
% José A. Alonso Jiménez
% Sevilla, 29 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Ni vale nada el fruto \\
     cogido sin sazón \dots \\
     Ni aunque te elogie un bruto \\
     ha de tener razón.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Arboles_con_n_elementos where
\end{code}
\end{comment}

La árboles binarios se pueden representar con
\begin{descripcion} 
   data Arbol a = H a
                | N a (Arbol a) (Arbol a)
     deriving (Show, Eq)
\end{descripcion}
   
Definir las funciones
\begin{descripcion} 
  arboles  :: Integer -> a -> [Arbol a]
  nArboles :: [Integer]
\end{descripcion}   
tales que
\begin{itemize}
\item (arboles n x) es la lista de todos los árboles binarios con n
  elementos iguales a x. Por ejemplo,
  \begin{descripcion} 
    λ> arboles 0 7
    []
    λ> arboles 1 7
    [H 7]
    λ> arboles 2 7
    []
    λ> arboles 3 7
    [N 7 (H 7) (H 7)]
    λ> arboles 4 7
    []
    λ> arboles 5 7
    [N 7 (H 7) (N 7 (H 7) (H 7)),N 7 (N 7 (H 7) (H 7)) (H 7)]
    λ> arboles 6 7
    []
    λ> arboles 7 7
    [N 7 (H 7) (N 7 (H 7) (N 7 (H 7) (H 7))),
     N 7 (H 7) (N 7 (N 7 (H 7) (H 7)) (H 7)),
     N 7 (N 7 (H 7) (H 7)) (N 7 (H 7) (H 7)),
     N 7 (N 7 (H 7) (N 7 (H 7) (H 7))) (H 7),
     N 7 (N 7 (N 7 (H 7) (H 7)) (H 7)) (H 7)]
  \end{descripcion} 
\item nArboles es la sucesión de los números de árboles con k elementos
  iguales a 7, con \(k \in \{1,3,5,\dots\}\). Por ejemplo,
  \begin{descripcion} 
    λ> take 14 nArboles
    [1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900]
    λ> nArboles !! 100
    896519947090131496687170070074100632420837521538745909320
    λ> length (show (nArboles !! 1000))
    598
  \end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Data.List (genericLength)

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- 1ª definición de arboles
-- ========================

arboles :: Integer -> a -> [Arbol a]
arboles 0 _ = []
arboles 1 x = [H x]
arboles n x = [N x i d | k <- [0..n-1],
                         i <- arboles k x,
                         d <- arboles (n-1-k) x]

-- 2ª definición de arboles
-- ========================

arboles2 :: Integer -> a -> [Arbol a]
arboles2 0 _ = []
arboles2 1 x = [H x]
arboles2 n x = [N x i d | k <- [1,3..n-1],
                          i <- arboles2 k x,
                          d <- arboles2 (n-1-k) x]
  
-- 1ª definición de nArboles
-- =========================

nArboles :: [Integer]
nArboles = [genericLength (arboles2 n 7) | n <- [1,3..]]

-- 2ª definición de nArboles
-- =========================

-- Con la definición anterior se observa que nArboles es
--    1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900
-- son los números de Catalan https://en.wikipedia.org/wiki/Catalan_number
-- Una forma de calcularlos (ver https://oeis.org/A000108) es
--     (2n)!/(n!(n+1)!)

nArboles2 :: [Integer]
nArboles2 =
  [factorial (2*n) `div` (factorial n * factorial (n+1)) | n <- [0..]]

factorial :: Integer -> Integer
factorial n = product [1..n]

-- 3ª definición de nArboles
-- =========================

-- 1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900

--  1
--  1 = 1*1
--  2 = 1*1  + 1*1
--  5 = 1*2  + 1*1 + 2*1
-- 14 = 1*5  + 1*2 + 2*1 + 5*1 
-- 42 = 1*14 + 1*5 + 2*2 + 5*1 + 14*1

nArboles3 :: [Integer]
nArboles3 = 1 : aux [1]
  where aux cs = c : aux (c:cs)
          where c = sum (zipWith (*) cs (reverse cs))  

-- Comparación de eficiencia
-- =========================

--    λ> length (show (nArboles !! 12))
--    6
--    (6.50 secs, 1,060,563,128 bytes)
--    λ> length (show (nArboles2 !! 12))
--    6
--    (0.01 secs, 108,520 bytes)
--    λ> length (show (nArboles3 !! 12))
--    6
--    (0.01 secs, 119,096 bytes)
--
--    λ> length (show (nArboles2 !! 1000))
--    598
--    (0.01 secs, 4,796,440 bytes)
--    λ> length (show (nArboles3 !! 1000))
--    598
--    (1.66 secs, 321,771,704 bytes)
\end{code} 
