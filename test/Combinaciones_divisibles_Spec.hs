module Combinaciones_divisibles_Spec (main, spec) where

import Combinaciones_divisibles
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSize) 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de tieneCombinacionDivisible" $
    verifica tieneCombinacionDivisible
  describe "Verificacion de tieneCombinacionDivisible2" $
    verifica tieneCombinacionDivisible2

verifica :: ([Int] -> Int -> Bool) -> Spec
verifica tieneCombinacionDivisible' = do
  it "e1" $ 
    tieneCombinacionDivisible' [1,3,4,6] 4  `shouldBe`  True
  it "e2" $ 
    tieneCombinacionDivisible' [1,3,9]   2  `shouldBe`  False
  modifyMaxSize (const 10) $ it "p1" $ property $
    prop_tieneCombinacionDivisible'
  where 
    prop_tieneCombinacionDivisible' :: [Int] -> Positive Int -> Bool
    prop_tieneCombinacionDivisible' xs (Positive m) =
      tieneCombinacionDivisible' xs m == tieneCombinacionDivisible xs m
  
