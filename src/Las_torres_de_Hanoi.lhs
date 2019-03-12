% Las_torres_de_Hanoi.hs
% Las torres de Hanói.
% José A. Alonso Jiménez <jalonso@us.es>
% Sevilla, 5 de marzo de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{En preguntar lo que sabes \\
     el tiempo no has de perder \dots \\
     Y a preguntas sin respuesta \\
     ¿quién te podrá responder?}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Las_torres_de_Hanoi where
\end{code}
\end{comment}

Las \href{http://bit.ly/1NwyvcA}{torres de Hanoi} es un rompecabeza que
consta de tres postes que llamaremos A, B y C. Hay N discos de
distintos tamaños en el poste A, de forma que no hay un disco situado
sobre otro de menor tamaño. Los postes B y C están vacíos. Sólo
puede moverse un disco a la vez y todos los discos deben de estar
ensartados en algún poste. Ningún disco puede situarse sobre otro de
menor tamaño. El problema consiste en colocar los N discos en el 
poste C.

Los postes se pueden representar mediante el siguiente tipo de datos
\begin{descripcion} 
  data Poste = A | B | C
    deriving Show
\end{descripcion} 

Definir las funciones
\begin{descripcion} 
  movimientos :: Integer -> [(Integer,Poste,Poste)]
  hanoi       :: Integer -> IO ()
\end{descripcion}   
tales que
\begin{itemize}
\item (movimientos n) es la lista de los movimientos para resolver el
  problema de las torres de hanoi con n discos. Por ejemplo,
\begin{descripcion}   
  λ> movimientos 1
  [(1,A,C)]
  λ> movimientos 2
  [(1,A,B),(2,A,C),(1,B,C)]
  λ> movimientos 3
  [(1,A,C),(2,A,B),(1,C,B),(3,A,C),(1,B,A),(2,B,C),(1,A,C)]
\end{descripcion} 
\item (hanoi n) escribe los mensajes de los movimientos para resolver el
  problema de las torres de hanoi con n discos. Por ejemplo,
\begin{descripcion}   
  λ> hanoi 3
  Mueve el disco 1 de A a C
  Mueve el disco 2 de A a B
  Mueve el disco 1 de C a B
  Mueve el disco 3 de A a C
  Mueve el disco 1 de B a A
  Mueve el disco 2 de B a C
  Mueve el disco 1 de A a C
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
data Poste = A | B | C
  deriving (Eq, Show)

movimientos :: Integer -> [(Integer,Poste,Poste)]
movimientos n = aux n A B C
  where  
    aux k a b c
      | k == 1    = [(1,a,c)]
      | otherwise = aux (k-1) a c b ++ (k,a,c) : aux (k-1) b a c

hanoi :: Integer -> IO ()
hanoi n = 
  putStrLn (unlines (map mensaje (movimientos n)))

-- (mensaje (n.x.y)) es la cadena indicando que el disco n se ha movido
-- desde el poste x al poste y. Por ejemplo, 
--    λ> mensaje (1,A,B)
--    "Mueve el disco 1 de A a B"
mensaje :: (Integer,Poste,Poste) -> String
mensaje (n,x,y) =
  "Mueve el disco " ++ show n ++ " de " ++ show x ++ " a " ++ show y
\end{code} 
