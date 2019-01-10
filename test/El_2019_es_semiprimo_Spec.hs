module El_2019_es_semiprimo_Spec (main, spec) where

import El_2019_es_semiprimo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esSemiprimo semiprimos" $
    verifica esSemiprimo semiprimos
  describe "Verificacion de esSemiprimo semiprimos2" $
    verifica esSemiprimo2 semiprimos
  describe "Verificacion de esSemiprimo semiprimos3" $
    verifica esSemiprimo3 semiprimos
  describe "Verificacion de esSemiprimo semiprimos4" $
    verifica esSemiprimo4 semiprimos

verifica :: (Integer -> Bool) -> [Integer] -> Spec
verifica f g = do
  it "e1" $
     esSemiprimo' 26          `shouldBe`  True
  it "e2" $
     esSemiprimo' 49          `shouldBe`  True
  it "e3" $
     esSemiprimo' 8           `shouldBe`  False
  it "e4" $
     esSemiprimo' 2019        `shouldBe`  True
  it "e5" $
     take 10 semiprimos'   `shouldBe`  [4,6,9,10,14,15,21,22,25,26]
  it "p1" $ property $
     \n -> n >= 0 ==> esSemiprimo' n == esSemiprimo4 n
  where
   esSemiprimo' = f
   semiprimos'  = g
  
   
