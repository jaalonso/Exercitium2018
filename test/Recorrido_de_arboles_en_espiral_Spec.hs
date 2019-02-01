module Recorrido_de_arboles_en_espiral_Spec (main, spec) where

import Recorrido_de_arboles_en_espiral
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de espiral" $
    verifica espiral
  describe "Verificacion de espiral2" $
    verifica espiral2
  describe "Verificacion de espiral3" $
    verifica espiral3
  describe "Verificacion de espiral4" $
    verifica espiral4
  describe "Verificacion de espiral5" $
    verifica espiral5
  describe "Verificacion de espiral6" $
    verifica espiral6

verifica :: (Arbol Int -> [Int]) -> Spec
verifica f = do
  it "e1" $
    espiral' ej1  `shouldBe`  [1,2,3,7,6,5,4]
  it "e2" $
    espiral' ej2  `shouldBe`  [1,8,3,6,5,4]
  it "e3" $
    espiral' ej3  `shouldBe`  [1,8,3,7,6,5,4]
  where espiral' = f

  
