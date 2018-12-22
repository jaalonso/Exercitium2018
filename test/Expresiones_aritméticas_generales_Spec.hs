module Expresiones_aritméticas_generales_Spec (main, spec) where

import Expresiones_aritméticas_generales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de valor" $
    verifica valor

verifica :: (Expresion -> Int) -> Spec
verifica f = do
  it "e1" $
    valor' (S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1])
     `shouldBe` 41
  where valor' = f


