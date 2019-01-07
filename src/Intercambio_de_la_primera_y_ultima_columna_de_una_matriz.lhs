% Intercambio_de_la_primera_y_ultima_columna_de_una_matriz.lhs
% Intercambio de la primera y última columna de una matriz.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 17 de diciembre de 2018
% ---------------------------------------------------------------------

\epigraph{\textit{¡Que difícil es, \\
    cuando todo baja \\
    no bajar también!}}
{Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Intercambio_de_la_primera_y_ultima_columna_de_una_matriz where    
\end{code}
\end{comment}

Las matrices se pueden representar mediante listas de listas. Por
ejemplo, la matriz
\begin{descripcion}
  8 9 7 6
  4 7 6 5
  3 2 1 8
\end{descripcion}   
se puede representar por la lista
\begin{descripcion}
  [[8,9,7,6],[4,7,6,5],[3,2,1,8]]
\end{descripcion}
 
Definir la función
\begin{descripcion}
  intercambia :: [[a]] -> [[a]]
\end{descripcion}
tal que (intercambia xss) es la matriz obtenida intercambiando la
primera y la última columna de xss. Por ejemplo,
\begin{descripcion}
  λ> intercambia [[8,9,7,6],[4,7,6,5],[3,2,1,8]]
  [[6,9,7,8],[5,7,6,4],[8,2,1,3]]
\end{descripcion}

\section*{Soluciones}

\begin{code}
intercambia :: [[a]] -> [[a]]
intercambia = map intercambiaL

-- (intercambiaL xs) es la lista obtenida intercambiando el primero y el
-- último elemento de xs. Por ejemplo,
--    intercambiaL [8,9,7,6]  ==  [6,9,7,8]
intercambiaL :: [a] -> [a]
intercambiaL xs =
  last xs : tail (init xs) ++ [head xs]
\end{code}
