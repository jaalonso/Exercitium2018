module Digitos_pares_de_cuadrados_Spec (main, spec) where

import Digitos_pares_de_cuadrados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de digitosPosParesCuadrado invDigitosPosParesCuadrado" $
    verifica digitosPosParesCuadrado invDigitosPosParesCuadrado
  describe "Verificacion de digitosPosParesCuadrado invDigitosPosParesCuadrado2" $
    verifica digitosPosParesCuadrado invDigitosPosParesCuadrado2


verifica :: (Integer -> ([Integer],Int))
            -> (([Integer],Int) -> [Integer])
            -> Spec
verifica f g = do
  it "e1" $
     digitosPosParesCuadrado' 8     `shouldBe`  ([6],2)
  it "e2" $
     digitosPosParesCuadrado' 14    `shouldBe`  ([1,6],3)
  it "e3" $
     digitosPosParesCuadrado' 36    `shouldBe`  ([1,9],4)
  it "e4" $
     digitosPosParesCuadrado' 116   `shouldBe`  ([1,4,6],5)
  it "e5" $
     digitosPosParesCuadrado' 2019  `shouldBe`  ([4,7,3,1],7)
  it "e6" $
     invDigitosPosParesCuadrado' ([6],2)             `shouldBe`  [8]
  it "e7" $
     invDigitosPosParesCuadrado' ([1,6],3)           `shouldBe`  [14]
  it "e8" $
     invDigitosPosParesCuadrado' ([1,9],4)           `shouldBe`  [36]
  it "e9" $
     invDigitosPosParesCuadrado' ([1,4,6],5)         `shouldBe`  [116,136]
  it "e10" $
     invDigitosPosParesCuadrado' ([4,7,3,1],7)       `shouldBe`  [2019,2139,2231]
  it "e11" $
     invDigitosPosParesCuadrado' ([1,2],3)           `shouldBe`  []
  it "e12" $
     invDigitosPosParesCuadrado' ([1,2],4)           `shouldBe`  [32,35,39]
  where
    digitosPosParesCuadrado'    = f
    invDigitosPosParesCuadrado' = g

    
  
