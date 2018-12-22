module Menor_contenedor_de_primos_Spec (main, spec) where

import Menor_contenedor_de_primos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de menorContenedor" $
    verifica menorContenedor
  describe "Verificacion de menorContenedor2" $
    verifica menorContenedor2

verifica :: (Int -> Int) -> Spec
verifica f = do
  it "e1" $
    menorContenedor' 1  `shouldBe`  2
  it "e2" $
    menorContenedor' 2  `shouldBe`  23
  it "e3" $
    menorContenedor' 3  `shouldBe`  235
  it "e4" $
    menorContenedor' 4  `shouldBe`  2357
  it "e5" $
    menorContenedor' 5  `shouldBe`  112357
  it "e6" $
    menorContenedor' 6  `shouldBe`  113257
  where menorContenedor' = f
  
