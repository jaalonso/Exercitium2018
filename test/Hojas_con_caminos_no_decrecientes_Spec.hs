module Hojas_con_caminos_no_decrecientes_Spec (main, spec) where

import Hojas_con_caminos_no_decrecientes
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de hojasEnNoDecreciente" $
    verifica hojasEnNoDecreciente
  describe "Verificacion de hojasEnNoDecreciente2" $
    verifica hojasEnNoDecreciente2

verifica :: (Arbol -> [Int]) -> Spec
verifica hojasEnNoDecreciente' = do
  it "e1" $
    sort (hojasEnNoDecreciente' ej1)  `shouldBe`  [4,5,7]
  it "e2" $
    sort (hojasEnNoDecreciente' ej2)  `shouldBe`  [4,6,8]
  it "e3" $
    hojasEnNoDecreciente' ej3  `shouldBe`  []
