module Suma_de_segmentos_iniciales_Spec (main, spec) where

import Suma_de_segmentos_iniciales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de sumaSegmentosIniciales" $
    verifica sumaSegmentosIniciales
  describe "Verificacion de sumaSegmentosIniciales2" $
    verifica sumaSegmentosIniciales2
  describe "Verificacion de sumaSegmentosIniciales3" $
    verifica sumaSegmentosIniciales3

verifica :: ([Integer] -> Integer) -> Spec
verifica sumaSegmentosIniciales' = do 
  it "e1" $
    sumaSegmentosIniciales [3,1,2,5] `shouldBe` 24
  it "e2" $
    [sumaSegmentosIniciales' [1..k] | k <- [10..19]] `shouldBe`
    [220,286,364,455,560,680,816,969,1140,1330]
  it "p1" $ property $
    \ xs -> sumaSegmentosIniciales' xs == sumaSegmentosIniciales3 xs

  
