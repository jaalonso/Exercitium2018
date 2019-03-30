module Siguiente_mayor_Spec (main, spec) where

import Siguiente_mayor
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de siguienteMayor" $
    verifica siguienteMayor
  describe "Verificacion de siguienteMayor2" $
    verifica siguienteMayor2
  describe "Verificacion de siguienteMayor3" $
    verifica siguienteMayor3

verifica :: ([Int] -> [Maybe Int]) -> Spec
verifica siguienteMayor' = do
  it "e1" $
    siguienteMayor' [4,5,2,3,9] `shouldBe`
    [Just 5,Just 9,Just 3,Just 9,Nothing]
  it "e2" $
    siguienteMayor' [9,5,2,3,4] `shouldBe`
    [Nothing,Nothing,Just 3,Just 4,Nothing]
  it "e3" $
    siguienteMayor' [9,5,2,2,4] `shouldBe`
    [Nothing,Nothing,Just 4,Just 4,Nothing]
  
