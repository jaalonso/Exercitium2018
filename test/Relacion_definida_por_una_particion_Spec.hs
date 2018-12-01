module Relacion_definida_por_una_particion_Spec (main, spec) where

import Relacion_definida_por_una_particion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de relacionados" $
    verifica relacionados
  describe "Verificacion de relacionados2" $
    verifica relacionados2
  describe "Verificacion de relacionados3" $
    verifica relacionados3
  describe "Verificacion de relacionados4" $
    verifica relacionados4

verifica :: ([[Int]] -> Int -> Int -> Bool) -> Spec
verifica f = do
  it "e1" $
    relacionados [[1,3],[2],[9,5,7]] 7 9  `shouldBe`  True
  it "e2" $
    relacionados [[1,3],[2],[9,5,7]] 3 9  `shouldBe`  False
  it "e3" $
    relacionados [[1,3],[2],[9,5,7]] 4 9  `shouldBe`  False
  where relacionados = f
  
