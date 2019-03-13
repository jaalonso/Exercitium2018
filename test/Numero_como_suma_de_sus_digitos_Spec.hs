module Numero_como_suma_de_sus_digitos_Spec (main, spec) where

import Numero_como_suma_de_sus_digitos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de minimoSumandosDigitos" $
    verifica minimoSumandosDigitos

verifica :: (Integer -> Integer) -> Spec
verifica minimoSumandosDigitos' = do
  it "e1" $
    minimoSumandosDigitos' 23    `shouldBe`  8
  it "e2" $
    minimoSumandosDigitos' 232   `shouldBe`  78
  it "e3" $
    minimoSumandosDigitos' 2323  `shouldBe`  775
  it "p1" $ property $
    \n -> n > 0 ==>
          minimoSumandosDigitos' n == minimoSumandosDigitos n

  
