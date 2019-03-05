module Particiones_de_un_conjunto_Spec (main, spec) where

import Particiones_de_un_conjunto
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de particiones" $
    verifica particiones
  describe "Verificacion de particiones2" $
    verifica particiones2
  describe "Verificacion de particiones3" $
    verifica particiones3

verifica :: ([Int] -> [[[Int]]]) -> Spec
verifica particiones' = do
  it "e1" $
    particiones'' [1,2] `shouldBe`
    [[[1],[2]],[[1,2]]]
  it "e2" $
    particiones'' [1,2,3] `shouldBe` 
    [[[1],[2],[3]],[[1],[2,3]],[[1,2],[3]],[[1,2,3]],[[1,3],[2]]]
  it "e3" $
    particiones'' [1..5] `shouldBe` 
    (sort . map sort . particiones) [1..5]
  where particiones'' = ordenada . particiones'
  
