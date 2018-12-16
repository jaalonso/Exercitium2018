module Posiciones_en_arboles_binarios_Spec (main, spec) where

import Posiciones_en_arboles_binarios
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de posiciones" $
    verifica posiciones
  describe "Verificacion de posiciones" $
    verifica posiciones2
  describe "Verificacion de posiciones" $
    verifica posiciones3

verifica :: (Int -> Arbol Int -> [Posicion]) -> Spec
verifica f = do
  it "e1" $
    posiciones' 0 ejArbol  `shouldBe`  [[I],[I,D],[D,I]]
  it "e2" $
    posiciones' 2 ejArbol  `shouldBe`  [[I,I,I]]
  it "e3" $
    posiciones' 3 ejArbol  `shouldBe`  [[],[D,D]]
  it "e4" $
    posiciones' 4 ejArbol  `shouldBe`  [[I,I,D]]
  it "e5" $
    posiciones' 5 ejArbol  `shouldBe`  [[I,I],[D]]
  it "e6" $
    posiciones' 1 ejArbol  `shouldBe`  ([] :: [Posicion])
  it "p1" $ property $
    \a -> and [ posiciones' n a == posiciones2 n a
              | n <- take 3 (elementos a)]
  where posiciones' = f
  
