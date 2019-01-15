module Minimo_producto_escalar_Spec (main, spec) where

import Minimo_producto_escalar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de menorProductoEscalar" $
    verifica menorProductoEscalar
  describe "Verificacion de menorProductoEscalar2" $
    verifica menorProductoEscalar2
  describe "Verificacion de menorProductoEscalar3" $
    verifica menorProductoEscalar3

verifica :: ([Integer] -> [Integer] -> Integer) -> Spec
verifica f = do
  it "e1" $
   menorProductoEscalar' [3,2,5]  [1,4,6]  `shouldBe` 29
  it "e2" $
   menorProductoEscalar' [3,2,5]  [1,4,-6] `shouldBe` -19
  it "e3" $
   menorProductoEscalar' [0..5]   [0..5]   `shouldBe` 20
  where menorProductoEscalar' = f
