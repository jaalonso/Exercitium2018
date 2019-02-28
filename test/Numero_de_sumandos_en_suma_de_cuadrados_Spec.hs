module Numero_de_sumandos_en_suma_de_cuadrados_Spec (main, spec) where

import Numero_de_sumandos_en_suma_de_cuadrados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ordenLagrange" $
    verifica ordenLagrange
  describe "Verificacion de ordenLagrange2" $
    verifica ordenLagrange2

verifica :: (Integer -> Int) -> Spec
verifica ordenLagrange' = do
  it "e1" $
    ordenLagrange' 16      `shouldBe`  1
  it "e2" $
     ordenLagrange' 29     `shouldBe`  2
  it "e3" $
     ordenLagrange' 14     `shouldBe`  3
  it "e4" $
     ordenLagrange' 15     `shouldBe`  4
  
