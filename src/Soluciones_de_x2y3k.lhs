% Soluciones_de_x^2=y^3=k.lhs
% Soluciones de x² = y³ = k.
% José A. Alonso Jiménez
% Sevilla, 22 de enero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{ Leyendo a Cervantes me parece comprenderlo todo.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Soluciones_de_x2y3k where
\end{code}
\end{comment}

Definir la función
\begin{descripcion} 
  soluciones :: [(Integer,Integer,Integer)]
\end{descripcion} 
tal que sus elementos son las ternas (x,y,k) de soluciones del
sistema \(x^2 = y^3 = k\). Por ejemplo,
\begin{descripcion} 
  λ> take 6 soluciones
  [(0,0,0),(-1,1,1),(1,1,1),(-8,4,64),(8,4,64),(-27,9,729)]
  λ> soluciones !! (6*10^5+6) 
  (27000810008100027,90001800009,729043741093514580109350437400729)
\end{descripcion}

\section*{Soluciones}

\begin{code} 
-- 1ª solución
-- ===========

soluciones :: [(Integer,Integer,Integer)]
soluciones = [(n^3, n^2, n^6) | n <- enteros]

-- enteros es la lista ordenada de los números enteros. Por ejemplo,
--    λ> take 20 enteros
--    [0,-1,1,-2,2,-3,3,-4,4,-5,5,-6,6,-7,7,-8,8,-9,9,-10]
enteros :: [Integer]
enteros = 0 : concat [[-x,x] | x <- [1..]]

-- 2ª solución
-- ===========

soluciones2 :: [(Integer,Integer,Integer)]
soluciones2 = [(x^3,x^2,x^6) | x <- 0 : aux 1]
  where aux n  = -n : n : aux (n+1)

-- 3ª solución
-- ===========

soluciones3 :: [(Integer,Integer,Integer)]
soluciones3 =
  (0,0,0) : [(x,y,k) | k <- [n^6 | n <- [1..]]  
                     , let Just x' = raiz 2 k
                     , let Just y  = raiz 3 k
                     , x <- [-x',x']]

-- (raiz n x) es es justo la raíz n-ésima del número natural x, si x es
-- una potencia n-ésima y Nothing en caso contrario. Por ejemplo,
--    raiz 2 16   ==  Just 4
--    raiz 3 216  ==  Just 6
--    raiz 5 216  ==  Nothing
raiz :: Int -> Integer -> Maybe Integer 
raiz _ 1 = Just 1
raiz n x = aux (0,x)
    where aux (a,b) | d == x    = Just c
                    | c == a    = Nothing
                    | d < x     = aux (c,b)
                    | otherwise = aux (a,c) 
              where c = (a+b) `div` 2
                    d = c^n

-- Comparación de eficiencia
-- =========================

--    λ> soluciones !! (6*10^5+6)
--    (27000810008100027,90001800009,729043741093514580109350437400729)
--    (1.87 secs, 247,352,728 bytes)
--    λ> soluciones2 !! (6*10^5+6)
--    (27000810008100027,90001800009,729043741093514580109350437400729)
--    (1.44 secs, 243,012,936 bytes)
--    λ> soluciones3 !! (6*10^5+6)
--    (27000810008100027,90001800009,729043741093514580109350437400729)
--    (0.84 secs, 199,599,664 bytes)
\end{code} 
