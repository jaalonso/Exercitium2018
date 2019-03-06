module Numero_de_particiones_de_un_conjunto_Spec (main, spec) where

import Numero_de_particiones_de_un_conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nParticiones" $
    verifica nParticiones
  describe "Verificacion de nParticiones2" $
    verifica nParticiones2
  describe "Verificacion de nParticiones3" $
    verifica nParticiones3
  describe "Verificacion de nParticiones4" $
    verifica nParticiones4

verifica :: ([Int] -> Integer) -> Spec
verifica nParticiones' = do
  it "e1" $
    nParticiones' [1,2]   `shouldBe` 2
  it "e2" $
    nParticiones' [1,2,3] `shouldBe` 5
  it "e3" $  
    [nParticiones' [1..n] | n <- [1..8]] `shouldBe` [1,2,5,15,52,203,877,4140]


  
