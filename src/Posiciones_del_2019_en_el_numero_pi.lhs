% Posiciones_del_2019_en_el_numero_pi.lhs
% Posiciones del 2019 en el número pi
% José A. Alonso Jiménez
% Sevilla, 17 de enero de 2019
% ---------------------------------------------------------------------

\epigraph {\textit{Aprendió tantas cosas, que no tuvo tiempo para pensar
    en ninguna de ellas.}}  {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Posiciones_del_2019_en_el_numero_pi where
\end{code}
\end{comment}

El fichero \href{http://bit.ly/2HuygTl}{Digitos\_de\_pi.txt} contiene el número
pi con un millón de decimales; es decir,
\[3.1415926535897932384626433832 \dots 83996346460422090106105779458151\]

Definir la función
\begin{descripcion} 
  posiciones :: String -> Int -> IO [Int]
\end{descripcion} 
tal que (posicion cs k) es es la lista de las posiciones iniciales de
cs en la sucesión formada por los k primeros dígitos decimales del
número pi. Por ejemplo,
\begin{descripcion} 
  λ> posiciones "141" 1000
  [0,294]
  λ> posiciones "4159" 10000
  [1,5797,6955,9599]
\end{descripcion}

Calcular la primera posición de 2019 en los decimales de pi  y el
número de veces que aparece 2019 en en el primer millón de decimales
de pi.

\section*{Soluciones}

\begin{code} 
import Data.List ( isPrefixOf
                 , findIndices
                 , tails  
                 )

-- 1ª definición
-- =============

posiciones :: String -> Int -> IO [Int]
posiciones cs k = do
  ds <- readFile "Digitos_de_pi.txt"
  return (posicionesEnLista cs (take (k-1) (drop 2 ds)))

--    posicionesEnLista "23" "234235523"  ==  [0,3,7]
posicionesEnLista :: Eq a => [a] -> [a] -> [Int]
posicionesEnLista xs ys = reverse (aux ys 0 [])
  where aux []      _ ns = ns
        aux (y:ys') n ns | xs `isPrefixOf` (y:ys') = aux ys' (n+1) (n:ns)
                         | otherwise               = aux ys' (n+1) ns

-- 2ª definición
-- =============

posiciones2 :: String -> Int -> IO [Int]
posiciones2 cs k = do
  ds <- readFile "Digitos_de_pi.txt"
  return (findIndices (cs `isPrefixOf`) (tails (take (k-1) (drop 2 ds))))

-- Comparación de eficiencia
-- =========================

--    λ> length <$> posiciones "2019" (10^6)
--    112
--    (1.73 secs, 352,481,272 bytes)
--    λ> length <$> posiciones2 "2019" (10^6)
--    112
--    (0.16 secs, 144,476,384 bytes)

-- El cálculo es
--    λ> ps <- posiciones "2019" (10^6)
--    λ> head ps
--    243
--    λ> length ps
--    112
-- Por tanto, la posición de la primera ocurrencia es 243 y hay 112
-- ocurrencias. Otra forma de hacer los cálculos anteriores es
--    λ> head <$> posiciones "2019" (10^6)
--    243
--    λ> length <$> posiciones "2019" (10^6)
--    112
\end{code}  
