module Posiciones_en_arboles_binarios_completos_Spec (main, spec) where

import Posiciones_en_arboles_binarios_completos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de posicionDeElemento" $
    verifica posicionDeElemento
  describe "Verificacion de posicionDeElemento2" $
    verifica posicionDeElemento2
  describe "Verificacion de posicionDeElemento3" $
    verifica posicionDeElemento3
  describe "Verificacion de posicionDeElemento4" $
    verifica posicionDeElemento4

verifica :: (Integer -> Posicion) -> Spec
verifica f = do
  it "e1" $
    posicionDeElemento' 6  `shouldBe`  [D,I]
  it "e2" $
    posicionDeElemento' 7  `shouldBe`  [D,D]
  it "e3" $
    posicionDeElemento' 9  `shouldBe`  [I,I,D]
  it "e4" $
    posicionDeElemento' 1  `shouldBe`  []
  it "p1" $ property $
    \n -> n > 0 ==> posicionDeElemento' n == posicionDeElemento4 n 
  where posicionDeElemento' = f
  
