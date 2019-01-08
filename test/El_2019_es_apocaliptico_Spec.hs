module El_2019_es_apocaliptico_Spec (main, spec) where

import El_2019_es_apocaliptico
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esApocaliptico apocalipticos posicionApocaliptica  " $
    verifica esApocaliptico apocalipticos posicionApocaliptica  
  describe "Verificacion de esApocaliptico apocalipticos posicionApocaliptica 2" $
    verifica esApocaliptico2 apocalipticos2 posicionApocaliptica2

verifica :: (Integer -> Bool) -> [Integer] -> (Integer -> Maybe Int) -> Spec
verifica f1 f2 f3 = do
  it "e1" $
     esApocaliptico' 157   `shouldBe`  True
  it "e2" $
     esApocaliptico' 2019  `shouldBe`  True
  it "e3" $
     esApocaliptico' 2018  `shouldBe`  False
  it "e4" $
     take 9 apocalipticos'  `shouldBe`  [157,192,218,220,222,224,226,243,245]
  it "e5" $
     posicionApocaliptica' 157   `shouldBe`  Just 0
  it "e6" $
     posicionApocaliptica' 2019  `shouldBe`  Just 450
  it "e7" $
     posicionApocaliptica' 2018  `shouldBe`  Nothing
  it "p1" $ property $
     \n -> n > 0 ==> posicionApocaliptica' n == posicionApocaliptica n
  where
   esApocaliptico'       = f1 
   apocalipticos'        = f2
   posicionApocaliptica' = f3 
  
