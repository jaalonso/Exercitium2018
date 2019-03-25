module Permutacion_de_consecutivos_Spec (main, spec) where

import Permutacion_de_consecutivos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de permutaConsecutivos" $
    verifica permutaConsecutivos
  describe "Verificacion de permutaConsecutivos2" $
    verifica permutaConsecutivos2

verifica :: ([Int] -> [Int]) -> Spec
verifica permutaConsecutivos' = do
  it "e1" $
    permutaConsecutivos' [1..8] `shouldBe`  [2,1,4,3,6,5,8,7]
  it "e2" $
    permutaConsecutivos' [1..9] `shouldBe`  [2,1,4,3,6,5,8,7,9]
  
