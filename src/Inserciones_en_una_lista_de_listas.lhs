% Inserciones_en_una_lista_de_listas.lhs
% Inserciones en una lista de listas.
% José A. Alonso Jiménez
% Sevilla, 25 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{\dots De la mar al percepto, \\
     del percepto al concepto, \\
     del concepto a la idea \\
     -- ¡oh, la linda tarea! -- \\
     de la idea a la mar. \\
     ¡Y otra vez al empezar!}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Inserciones_en_una_lista_de_listas where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  inserta :: a -> [[a]] -> [[[a]]]
\end{descripcion}   
tal que (inserta x yss) es la lista obtenida insertando x en cada uno
de los elementos de yss. Por ejemplo,
\begin{descripcion} 
  λ> inserta 1 [[2,3],[4],[5,6,7]]
  [[[1,2,3],[4],[5,6,7]],[[2,3],[1,4],[5,6,7]],[[2,3],[4],[1,5,6,7]]]
  λ> inserta 'a' ["hoy","es","lunes"]
  [["ahoy","es","lunes"],["hoy","aes","lunes"],["hoy","es","alunes"]]
\end{descripcion}
 
\section*{Soluciones}

\begin{code}    
-- 1ª solución
inserta :: a -> [[a]] -> [[[a]]]
inserta _ []       = []
inserta x (ys:yss) = ((x:ys):yss) : [ys : zs | zs <- inserta x yss] 

-- 2ª solución
inserta2 :: a -> [[a]] -> [[[a]]]
inserta2 _ []       = []
inserta2 x (ys:yss) = ((x:ys):yss) : map (ys:) (inserta2 x yss) 
\end{code} 
