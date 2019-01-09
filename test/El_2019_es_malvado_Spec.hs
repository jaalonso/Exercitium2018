module El_2019_es_malvado_Spec (main, spec) where

import El_2019_es_malvado
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esMalvado malvados posicionMalvada" $
    verifica esMalvado malvados posicionMalvada
  describe "Verificacion de esMalvado malvados posicionMalvada2" $
    verifica esMalvado2 malvados2 posicionMalvada2
  describe "Verificacion de esMalvado malvados posicionMalvada3" $
    verifica esMalvado3 malvados2 posicionMalvada2

verifica :: (Integer -> Bool) -> [Integer] -> (Integer -> Maybe Int) -> Spec
verifica f1 f2 f3 = do
  it "e1" $
     esMalvado'' 6              `shouldBe`  True
  it "e2" $
     esMalvado'' 7              `shouldBe`  False
  it "e3" $
     esMalvado'' 2019           `shouldBe`  True
  it "e4" $
     take 20 malvados'
      `shouldBe` [0,3,5,6,9,10,12,15,17,18,20,23,24,27,29,30,33,34,36,39]
  it "e5" $
     posicionMalvada' 6        `shouldBe`  Just 3
  it "e6" $
     posicionMalvada' 2019     `shouldBe`  Just 1009
  it "e7" $
     posicionMalvada' 2018     `shouldBe`  Nothing
  it "p1" $ property $
     \n -> n > 0 ==> posicionMalvada' n == posicionMalvada2 n
  where
    esMalvado''       = f1
    malvados'        = f2
    posicionMalvada' = f3

  
