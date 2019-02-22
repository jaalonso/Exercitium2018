module Sucesion_de_Cantor_de_numeros_innombrables_Spec (main, spec) where

import Sucesion_de_Cantor_de_numeros_innombrables
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de sucCantor" $
    verifica sucCantor1
  describe "Verificacion de sucCantor2" $
    verifica sucCantor2
  describe "Verificacion de sucCantor3" $
    verifica sucCantor3

verifica :: [Integer] -> Spec
verifica sucCantor' = do
  it "e1" $
    take 20 sucCantor' `shouldBe`
    [1,2,3,4,5,6,1,8,9,10,11,12,13,2,15,16,3,18,19,20]

  
