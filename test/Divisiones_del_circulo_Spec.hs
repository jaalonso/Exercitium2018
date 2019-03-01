module Divisiones_del_circulo_Spec (main, spec) where

import Divisiones_del_circulo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de numeroFormas" $
    verifica numeroFormas
  describe "Verificacion de numeroFormas2" $
    verifica numeroFormas2

verifica :: (Integer -> Integer) -> Spec
verifica numeroFormas' = do
  it "e1" $
    numeroFormas' 1   `shouldBe`  1
  it "e2" $
    numeroFormas' 2   `shouldBe`  2
  it "e3" $
    numeroFormas' 4   `shouldBe`  14
  
