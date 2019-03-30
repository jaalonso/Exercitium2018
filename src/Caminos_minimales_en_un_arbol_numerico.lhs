% Caminos_minimales_en_un_arbol_numerico.lhs
% Caminos minimales en un arbol numérico
% José A. Alonso Jiménez
% Sevilla, 19 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Tras el vivir y el soñar, \\
     está lo que más importa: \\
     despertar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Caminos_minimales_en_un_arbol_numerico where
\end{code}
\end{comment}

En la librería \href{http://bit.ly/2mLEVg4}{Data.Tree} se definen los
árboles y los bosques como sigue
\begin{descripcion} 
  data Tree a   = Node a (Forest a)
  type Forest a = [Tree a]
\end{descripcion}   

Se pueden definir árboles. Por ejemplo,
\begin{descripcion} 
  ej = Node 3 [Node 5 [Node 9 []], Node 7 []]
\end{descripcion}   
Y se pueden dibujar con la función drawTree. Por ejemplo,
\begin{descripcion} 
  λ> putStrLn (drawTree (fmap show ej))
  3
  |
  +- 5
  |  |
  |  `- 9
  |
  `- 7
\end{descripcion} 

Los \textbf{mayores divisores} de un número x son los divisores u tales que
u > 1 y existe un v tal que 1 < v <= u y u*v = x.  Por ejemplo, los
mayores divisores de 24 son 12, 8 y 6. 

El \textbf{árbol de los predecesores y mayores divisores} de un número x es el
árbol cuya raíz es x y los sucesores de cada nodo y > 1 es el conjunto
formado por y-1 junto con los mayores divisores de y. Los nodos con
valor 1 no tienen sucesores. Por ejemplo, el árbol de los
predecesores y mayores divisores del número 6 es
\begin{descripcion} 
       6
      / \
     5   3 
     |   |
     4   2
    / \  |
   3   2 1 
   |   | 
   2   1
   |
   1
\end{descripcion}
 
Definir las siguientes funciones
\begin{descripcion}  
  mayoresDivisores :: Int -> [Int]
  arbol            :: Int -> Tree Int
  caminos          :: Int -> [[Int]]
  caminosMinimales :: Int -> [[Int]]
\end{descripcion} 
tales que
\begin{itemize}
\item (mayoresDivisores x) es la lista de los mayores divisores de x. Por
  ejemplo,
\begin{descripcion}   
  mayoresDivisores 24  ==  [12,8,6]
  mayoresDivisores 16  ==  [8,4]
  mayoresDivisores 10  ==  [5]
  mayoresDivisores 17  ==  []
\end{descripcion} 
\item (arbol x) es el árbol de los predecesores y mayores divisores del
  número x. Por ejemplo,
\begin{descripcion}   
  λ> putStrLn (drawTree (fmap show (arbol 6)))
  6
  |
  +- 5
  |  |
  |  `- 4
  |     |
  |     +- 3
  |     |  |
  |     |  `- 2
  |     |     |
  |     |     `- 1
  |     |
  |     `- 2
  |        |
  |        `- 1
  |
  `- 3
     |
     `- 2
        |
        `- 1
\end{descripcion} 
\item (caminos x) es la lista de los caminos en el árbol de los
  predecesores y mayores divisores del número x. Por ejemplo,
\begin{descripcion}   
  λ> caminos 6
  [[6,5,4,3,2,1],[6,5,4,2,1],[6,3,2,1]]
\end{descripcion} 
\item (caminosMinimales x) es la lista de los caminos en de menor
  longitud en el árbol de los predecesores y mayores divisores del
  número x. Por ejemplo,
\begin{descripcion}   
  λ> caminosMinimales 6
  [[6,3,2,1]]
  λ> caminosMinimales 17
  [[17,16,4,2,1]]
  λ> caminosMinimales 50
  [[50,25,5,4,2,1],[50,10,9,3,2,1],[50,10,5,4,2,1]]
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code}      
import Data.Tree

mayoresDivisores :: Int -> [Int]
mayoresDivisores x =
  [max u v | u <- [2..floor (sqrt (fromIntegral x))]
           , x `mod` u == 0
           , let v = x `div` u]  

arbol :: Int -> Tree Int
arbol 1 = Node 1 []
arbol x = Node x (arbol (x-1) : [arbol y | y <- mayoresDivisores x])

caminos :: Int -> [[Int]]
caminos = caminosArbol . arbol

--    λ> caminosArbol (arbol 6)
--    [[6,5,4,3,2,1],[6,5,4,2,1],[6,3,2,1]]
caminosArbol :: Tree a -> [[a]]
caminosArbol (Node x []) = [[x]]
caminosArbol (Node x as) = [x:ys | ys <- caminosBosque as]

caminosBosque :: Forest a -> [[a]]
caminosBosque = concatMap caminosArbol

caminosMinimales :: Int -> [[Int]]
caminosMinimales x = [ys | ys <- yss, length ys == m]
  where yss = caminos x
        m   = minimum (map length yss)
\end{code} 
