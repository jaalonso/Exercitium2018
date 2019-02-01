% Recorrido_de_arboles_en_espiral.lhs
% Recorrido de árboles en espiral.
% José A. Alonso Jiménez 
% Sevilla, 25 de Febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{ Dice la monotonía \\
     del agua clara al caer: \\
     un día es como otro día; \\
     hoy es lo mismo que ayer.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Recorrido_de_arboles_en_espiral where
\end{code}
\end{comment}

Los árboles se pueden representar mediante el siguiente tipo de datos
\begin{descripcion} 
  data Arbol a = N a [Arbol a]
    deriving Show
\end{descripcion}   
Por ejemplo, los árboles
\begin{descripcion} 
         1             1             1  
        /  \          / \           / \ 
       /    \        8   3         8   3
      2      3          /|\       /|\  |
     / \    / \        4 5 6     4 5 6 7
    4   5  6   7
\end{descripcion} 
se representan por
\begin{descripcion} 
  ej1, ej2, ej3 :: Arbol Int
  ej1 = N 1 [N 2 [N 4 [], N 5 []], N 3 [N 6 [], N 7 []]]
  ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
  ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]

Definir la función
\begin{descripcion} 
  espiral :: Arbol a -> [a]
\end{descripcion} 
tal que (espiral x) es la lista de los nodos del árbol x recorridos
en espiral; es decir, la raíz de x, los nodos del primer nivel de
izquierda a derecha, los nodos del segundo nivel de derecha a
izquierda y así sucesivamente. Por ejemplo,
\begin{descripcion} 
  espiral ej1  ==  [1,2,3,7,6,5,4]
  espiral ej2  ==  [1,8,3,6,5,4]
  espiral ej3  ==  [1,8,3,7,6,5,4]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
data Arbol a = N a [Arbol a]
  deriving Show
           
ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 2 [N 4 [], N 5 []], N 3 [N 6 [], N 7 []]]
ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]

-- 1ª solución
-- ===========

espiral :: Arbol a -> [a]
espiral x =
  concat [f xs | (f,xs) <- zip (cycle [reverse,id]) (niveles x)]

-- (niveles x) es la lista de los niveles del árbol x. Por ejemplo, 
--    niveles ej1 == [[1],[8,3],[4]]
--    niveles ej2 == [[1],[8,3],[4,5,6]]
--    niveles ej3 == [[1],[8,3],[4,5,6,7]]
niveles :: Arbol a -> [[a]]
niveles x = takeWhile (not . null) [nivel n x | n <- [0..]]

-- (nivel n x) es el nivel de nivel n del árbol x. Por ejemplo,
--    nivel 0 ej1  ==  [1]
--    nivel 1 ej1  ==  [8,3]
--    nivel 2 ej1  ==  [4]
--    nivel 4 ej1  ==  []
nivel :: Int -> Arbol a ->  [a]
nivel 0 (N x _)  = [x]
nivel n (N _ xs) = concatMap (nivel (n-1)) xs

-- 2ª solución
-- ===========

espiral2 :: Arbol a -> [a]
espiral2 = 
  concat . zipWith ($) (cycle [reverse,id]) . niveles

-- 3ª solución
-- ===========

espiral3 :: Arbol a -> [a]
espiral3 = concat . zipWith ($) (cycle [reverse,id]) . niveles3

niveles3 :: Arbol a -> [[a]]
niveles3 t = map (map raiz)
           . takeWhile (not . null)
           . iterate (concatMap subBosque) $ [t]

raiz :: Arbol a -> a
raiz (N x _) = x

subBosque :: Arbol a -> [Arbol a]
subBosque (N _ ts) = ts

-- 4ª solución
-- ===========

espiral4 :: Arbol a -> [a]
espiral4 = concat . zipWith ($) (cycle [reverse,id]) . niveles4

niveles4 :: Arbol a -> [[a]]
niveles4 = map (map raiz)
         . takeWhile (not . null)
         . iterate (concatMap subBosque)
         . return  

-- 5ª definición
-- =============

espiral5 :: Arbol a -> [a]
espiral5 x = concat $ zipWith ($) (cycle [reverse,id]) $ niveles5 [x]

niveles5 :: [Arbol a] -> [[a]]
niveles5 [] = []
niveles5 xs = a : niveles5 (concat b)
  where (a,b) = unzip $ map (\(N x y) -> (x,y)) xs

-- 6ª definición
-- =============

espiral6 :: Arbol a -> [a]
espiral6 = concat . zipWith ($) (cycle [reverse,id]) . niveles5 . return
\end{code} 
