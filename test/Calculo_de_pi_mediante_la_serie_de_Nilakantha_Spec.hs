module Calculo_de_pi_mediante_la_serie_de_Nilakantha_Spec (main, spec) where

import Calculo_de_pi_mediante_la_serie_de_Nilakantha
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de aproximacionPi" $
    verifica aproximacionPi
  describe "Verificacion de aproximacionPi2" $
    verifica aproximacionPi2
  describe "Verificacion de aproximacionPi3" $
    verifica aproximacionPi3

verifica :: (Int -> Double) -> Spec
verifica f = do
  it "e1" $
    aproximacionPi' 0        `shouldBe`  3.0
  it "e2" $
    aproximacionPi' 1        `shouldBe`  3.1666666666666665
  it "e3" $
    aproximacionPi' 2        `shouldBe`  3.1333333333333333
  it "e4" $
    aproximacionPi' 3        `shouldBe`  3.145238095238095
  it "e5" $
    aproximacionPi' 4        `shouldBe`  3.1396825396825396
  it "e6" $
    aproximacionPi' 5        `shouldBe`  3.1427128427128426
  it "e7" $
    aproximacionPi' 10       `shouldBe`  3.1414067184965018
  where  
    aproximacionPi' = f

  
