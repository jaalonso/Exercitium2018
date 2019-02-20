% Calculo_de_pi_mediante_la_serie_de_Nilakantha.lhs
% Cálculo de pi mediante la serie de Nilakantha.
% José A. Alonso Jiménez
% Sevilla, 13 de febrero de 2019
% ---------------------------------------------------------------------

\epigraph
 {\textit{Bueno es saber que los vasos \\
     nos sirven para beber; \\
     lo malo es que no sabemos \\
     para que sirve la sed.}}
 {Antonio Machado}

\section*{Enunciado}

\begin{comment}
\begin{code}
module Calculo_de_pi_mediante_la_serie_de_Nilakantha where
\end{code}
\end{comment}

Una serie infinita para el cálculo de pi, publicada por
\href{http://bit.ly/2l84M1J}{Nilakantha} en el siglo XV, es
\[
  \pi = 3 
        + \frac{4}{2 \times 3 \times 4}
        - \frac{4}{4 \times 5 \times 6}
        + \frac{4}{6 \times 7 \times 8}
        - \frac{4}{8 \times 8 \times 10}
        + \dots
\]
            
Definir las funciones
\begin{descripcion} 
  aproximacionPi :: Int -> Double
  tabla          :: FilePath -> [Int] -> IO ()
\end{descripcion} 
tales que
\begin{itemize}
\item (aproximacionPi n) es la n-ésima aproximación de pi obtenida
  sumando los n primeros términos de la serie de Nilakantha. Por
  ejemplo,
\begin{descripcion}   
  aproximacionPi 0        ==  3.0
  aproximacionPi 1        ==  3.1666666666666665
  aproximacionPi 2        ==  3.1333333333333333
  aproximacionPi 3        ==  3.145238095238095
  aproximacionPi 4        ==  3.1396825396825396
  aproximacionPi 5        ==  3.1427128427128426
  aproximacionPi 10       ==  3.1414067184965018
  aproximacionPi 100      ==  3.1415924109719824
  aproximacionPi 1000     ==  3.141592653340544
  aproximacionPi 10000    ==  3.141592653589538
  aproximacionPi 100000   ==  3.1415926535897865
  aproximacionPi 1000000  ==  3.141592653589787
  pi                      ==  3.141592653589793
\end{descripcion} 
\item (tabla f ns) escribe en el fichero f las n--ésimas aproximaciones de
  pi, donde n toma los valores de la lista ns, junto con sus
  errores. Por ejemplo, al evaluar la expresión
\begin{descripcion}   
  tabla "AproximacionesPi.txt" [0,10..100]
\end{descripcion}   
  hace que el contenido del fichero "AproximacionesPi.txt" sea
\begin{descripcion} 
  +------+----------------+----------------+
  | n    | Aproximación   | Error          |
  +------+----------------+----------------+
  |    0 | 3.000000000000 | 0.141592653590 |
  |   10 | 3.141406718497 | 0.000185935093 |
  |   20 | 3.141565734659 | 0.000026918931 |
  |   30 | 3.141584272675 | 0.000008380915 |
  |   40 | 3.141589028941 | 0.000003624649 |
  |   50 | 3.141590769850 | 0.000001883740 |
  |   60 | 3.141591552546 | 0.000001101044 |
  |   70 | 3.141591955265 | 0.000000698325 |
  |   80 | 3.141592183260 | 0.000000470330 |
  |   90 | 3.141592321886 | 0.000000331704 |
  |  100 | 3.141592410972 | 0.000000242618 |
  +------+----------------+----------------+
\end{descripcion}   
  al evaluar la expresión
\begin{descripcion}   
  tabla "AproximacionesPi.txt" [0,500..5000]
\end{descripcion}   
  hace que el contenido del fichero "AproximacionesPi.txt" sea
\begin{descripcion} 
  +------+----------------+----------------+
  | n    | Aproximación   | Error          |
  +------+----------------+----------------+
  |    0 | 3.000000000000 | 0.141592653590 |
  |  500 | 3.141592651602 | 0.000000001988 |
  | 1000 | 3.141592653341 | 0.000000000249 |
  | 1500 | 3.141592653516 | 0.000000000074 |
  | 2000 | 3.141592653559 | 0.000000000031 |
  | 2500 | 3.141592653574 | 0.000000000016 |
  | 3000 | 3.141592653581 | 0.000000000009 |
  | 3500 | 3.141592653584 | 0.000000000006 |
  | 4000 | 3.141592653586 | 0.000000000004 |
  | 4500 | 3.141592653587 | 0.000000000003 |
  | 5000 | 3.141592653588 | 0.000000000002 |
  +------+----------------+----------------+
\end{descripcion}   
\end{itemize}

\section*{Soluciones}

\begin{code} 
import Text.Printf

-- 1ª solución
-- ===========

aproximacionPi :: Int -> Double
aproximacionPi n = serieNilakantha !! n

serieNilakantha :: [Double]
serieNilakantha = scanl1 (+) terminosNilakantha

terminosNilakantha :: [Double]
terminosNilakantha = zipWith (/) numeradores denominadores
  where numeradores   = 3 : cycle [4,-4]
        denominadores = 1 : [n*(n+1)*(n+2) | n <- [2,4..]]

-- 2ª solución
-- ===========

aproximacionPi2 :: Int -> Double
aproximacionPi2 = aux 3 2 1
  where aux x _ _ 0 = x
        aux x y z m =
          aux (x+4/product[y..y+2]*z) (y+2) (negate z) (m-1)

-- 3ª solución
-- ===========

aproximacionPi3 :: Int -> Double
aproximacionPi3 x =
  3 + sum [(((-1)**(n+1))*4)/(2*n*(2*n+1)*(2*n+2))
          | n <- [1..fromIntegral x]]


-- Comparación de eficiencia
-- =========================

--    λ> aproximacionPi (10^6)
--    3.141592653589787
--    (1.35 secs, 729,373,160 bytes)
--    λ> aproximacionPi2 (10^6)
--    3.141592653589787
--    (2.96 secs, 2,161,766,096 bytes)
--    λ> aproximacionPi3 (10^6)
--    3.1415926535897913
--    (2.02 secs, 1,121,372,536 bytes)

-- Definicioń de tabla
-- ===================

tabla :: FilePath -> [Int] -> IO ()
tabla f ns = writeFile f (tablaAux ns)

tablaAux :: [Int] -> String
tablaAux ns =
     linea
  ++ cabecera
  ++ linea
  ++ concat [printf "| %4d | %.12f | %.12f |\n" n a e
            | n <- ns
            , let a = aproximacionPi n
            , let e = abs (pi - a)]
  ++ linea

linea :: String
linea = "+------+----------------+----------------+\n"

cabecera :: String
cabecera = "| n    | Aproximación   | Error          |\n"
\end{code} 
