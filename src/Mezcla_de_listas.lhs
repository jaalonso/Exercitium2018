% Mezcla_de_listas.lhs
% Mezcla de listas.
% José A. Alonso Jiménez
% Sevilla, 18 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Cuatro cosas tiene el hombre \\
     que no sirven en la mar: \\
     ancla, gobernalle y remos, \\
     y miedo de naufragar.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Mezcla_de_listas where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  mezcla :: [[a]] -> [a]
\end{descripcion} 
tal que (mezcla xss) es la lista tomando sucesivamente los elementos
de xss en la misma posición. Cuando una de las listas de xss es
vacía, se continua con las restantes. por ejemplo,
\begin{descripcion} 
  mezcla [[1,2],[3..7],[8..10]]            ==  [1,3,8,2,4,9,5,10,6,7]
  mezcla ["Estamos","en","2019"]           ==  "Ee2sn0t1a9mos"
  take 9 (mezcla [[3,6..],[5,7..],[0,1]])  ==  [3,5,0,6,7,1,9,9,12]
\end{descripcion}

\section*{Soluciones}

\begin{code} 
import Data.List (transpose)

-- 1ª solución
mezcla :: [[a]] -> [a]
mezcla xss = aux (filter (not . null) xss)
  where
    aux []  = []
    aux yss = map head yss ++ mezcla (map tail yss)

-- 2ª solución
mezcla2 :: [[a]] -> [a]
mezcla2 = aux . filter (not . null)
  where
    aux []  = []
    aux yss = map head yss ++ mezcla (map tail yss)

-- 3ª solución
mezcla3 :: [[a]] -> [a]
mezcla3 = concatMap primeros . takeWhile (not . null) . iterate restos
  where primeros = map head . filter (not . null)
        restos   = map tail . filter (not . null)

-- 4ª solución
mezcla4 :: [[a]] -> [a]
mezcla4 = concat . transpose
\end{code} 
