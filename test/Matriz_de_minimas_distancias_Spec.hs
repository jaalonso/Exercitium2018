module Matriz_de_minimas_distancias_Spec (main, spec) where

import Matriz_de_minimas_distancias
import Test.Hspec
import Data.Matrix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de minimasDistancias sumaMinimaDistanciasIdentidad" $
    verifica minimasDistancias sumaMinimaDistanciasIdentidad
  describe "Verificacion de minimasDistancias sumaMinimaDistanciasIdentidad2" $
    verifica minimasDistancias2 sumaMinimaDistanciasIdentidad2

verifica :: (Matrix Int -> Matrix Int) -> (Int -> Int) -> Spec
verifica minimasDistancias' sumaMinimaDistanciasIdentidad' = do
  it "e1" $
    compara (fromLists [[0,1,1],[0,0,1]])
  it "e2" $
    compara (fromLists [[0,0,1],[1,0,0]])
  it "e3" $
    compara (identity 5)
  it "e4" $
    sumaMinimaDistanciasIdentidad' 5 == 40
  where compara a =
          minimasDistancias' a == minimasDistancias2 a

  
