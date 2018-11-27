module Diferencia_simetrica_Spec (main, spec) where

import Diferencia_simetrica
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de diferenciaSimetrica" $
    verifica diferenciaSimetrica
  describe "Verificacion de diferenciaSimetrica2" $
    verifica diferenciaSimetrica2
  describe "Verificacion de diferenciaSimetrica3" $
    verifica diferenciaSimetrica3

verifica :: ([Int] -> [Int] -> [Int]) -> Spec
verifica f = do
  it "e1" $
    diferenciaSimetrica [2,5,3] [4,2,3,7]    `shouldBe`  [4,5,7]   
  it "e2" $
    diferenciaSimetrica [2,5,3] [5,2,3]      `shouldBe`  []        
  it "e3" $
    diferenciaSimetrica [2,5,2] [4,2,3,7]    `shouldBe`  [3,4,5,7] 
  it "e4" $
    diferenciaSimetrica [2,5,2] [4,2,4,7]    `shouldBe`  [4,5,7] 
  it "e5" $
    diferenciaSimetrica [2,5,2,4] [4,2,4,7]  `shouldBe`  [5,7]
  it "p1" $ property $
    prop_equivalencia f
  where diferenciaSimetrica xs ys = f xs ys

prop_equivalencia :: ([Int] -> [Int] -> [Int]) -> [Int] -> [Int] -> Bool
prop_equivalencia f xs ys =
  f xs ys == diferenciaSimetrica xs ys
