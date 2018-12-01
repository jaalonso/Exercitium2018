module Reconocimiento_de_particiones_Spec (main, spec) where

import Reconocimiento_de_particiones
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esParticion" $
    verifica esParticion
  describe "Verificacion de esParticion2" $
    verifica esParticion2
  describe "Verificacion de esParticion3" $
    verifica esParticion3

verifica :: ([[Int]] -> Bool) -> Spec
verifica f = do
  it "e1" $
    esParticion [[1,3],[2],[9,5,7]]  `shouldBe`  True
  it "e2" $
    esParticion [[1,3],[2],[9,5,1]]  `shouldBe`  False
  it "e3" $
    esParticion [[1,3],[],[9,5,7]]   `shouldBe`  False
  it "e4" $
    esParticion [[2,3,2],[4]]        `shouldBe`  True
  it "p1" $ property $
    \xss -> esParticion xss == esParticion3 xss
  where esParticion = f
  
