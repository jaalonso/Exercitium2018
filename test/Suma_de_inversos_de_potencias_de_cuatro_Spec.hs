module Suma_de_inversos_de_potencias_de_cuatro_Spec (main, spec) where

import Suma_de_inversos_de_potencias_de_cuatro
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de aproximacion" $
    verifica aproximacion
  describe "Verificacion de aproximacion2" $
    verifica aproximacion2

verifica :: (Double -> Int) -> Spec
verifica f = do
  it "e1" $
    aproximacion 0.001  `shouldBe`  4
  it "e2" $
    aproximacion 1e-3   `shouldBe`  4
  it "e3" $
    aproximacion 1e-6   `shouldBe`  9
  it "e4" $
    aproximacion 1e-20  `shouldBe`  26
  where aproximacion = f
  
