module Raiz_cubica_entera_Spec (main, spec) where

import Raiz_cubica_entera
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de raizCubicaEntera" $
    verifica raizCubicaEntera
  describe "Verificacion de raizCubicaEntera2" $
    verifica raizCubicaEntera2
  describe "Verificacion de raizCubicaEntera3" $
    verifica raizCubicaEntera3
  describe "Verificacion de raizCubicaEntera5" $
    verifica raizCubicaEntera5

verifica :: (Integer -> Maybe Integer) -> Spec
verifica f = do
  it "e1" $
    raizCubicaEntera 8 `shouldBe`  Just 2
  it "e2" $
    raizCubicaEntera 9 `shouldBe`  Nothing
  it "e3" $
    raizCubicaEntera 27 `shouldBe`  Just 3
  it "e4" $
    raizCubicaEntera 64 `shouldBe`  Just 4
  it "p1" $ property $
    \x -> x > 0 ==> raizCubicaEntera x == raizCubicaEntera3 x
  where raizCubicaEntera = f
  
