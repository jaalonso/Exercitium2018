module Aproximacion_entre_pi_y_e_Spec (main, spec) where

import Aproximacion_entre_pi_y_e
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de sumaTerminos" $
    verifica sumaTerminos aproximacion
  describe "Verificacion de sumaTerminos2" $
    verifica sumaTerminos2 aproximacion

verifica :: (Int -> Double)
            -> (Double -> Int)
            -> Spec
verifica f g = do
  it "e1" $
    sumaTerminos0 10     `shouldBe`  0.14687821811081034
  it "e2" $
    sumaTerminos0 100    `shouldBe`  0.15550948345688423
  it "e3" $
    sumaTerminos0 1000   `shouldBe`  0.15641637221314514
  it "e4" $
    sumaTerminos0 10000  `shouldBe`  0.15650751113789382
  it "e5" $
    aproximacion0 0.1     `shouldBe`  1
  it "e6" $
    aproximacion0 0.01    `shouldBe`  10
  it "e7" $
    aproximacion0 0.001   `shouldBe`  101
  it "e8" $ 
    and [sumaTerminos n == f n | n <- [0,10..100]]
  where sumaTerminos0 = f
        aproximacion0 = g
  
