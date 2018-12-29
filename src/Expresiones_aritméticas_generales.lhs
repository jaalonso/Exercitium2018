% Expresiones_aritméticas_generales.lhs
% Expresiones aritméticas generales.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 13 de Diciembre de 2018
% ---------------------------------------------------------------------

\section*{Enunciado}

\begin{comment}
\begin{code}
module Expresiones_aritméticas_generales where    
\end{code}
\end{comment}

Las expresiones aritméticas. generales se contruyen con las sumas
generales (sumatorios) y productos generales (productorios). Su tipo
es
\begin{descripcion}
   data Expresion = N Int
                  | S [Expresion]
                  | P [Expresion]
     deriving Show
\end{descripcion}
Por ejemplo, la expresión \verb|(2 * (1 + 2 + 1) * (2 + 3)) + 1| se
representa por \verb|S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1]|

Definir la función
\begin{descripcion}
   valor :: Expresion -> Int
\end{descripcion}
tal que (valor e) es el valor de la expresión e. Por ejemplo,
\begin{descripcion}
   λ> valor (S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1])
   41
\end{descripcion}

\section*{Soluciones}

\begin{code}
data Expresion = N Int
               | S [Expresion]
               | P [Expresion]
  deriving Show

valor :: Expresion -> Int
valor (N x)  = x
valor (S es) = sum (map valor es)
valor (P es) = product (map valor es)
\end{code}
