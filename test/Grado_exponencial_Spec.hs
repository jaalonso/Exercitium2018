module Grado_exponencial_Spec (main, spec) where

import Grado_exponencial
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de gradoExponencial" $
    verifica gradoExponencial
  describe "Verificacion de gradoExponencial2" $
    verifica gradoExponencial2
  describe "Verificacion de gradoExponencial3" $
    verifica gradoExponencial3

verifica :: (Integer -> Integer) -> Spec
verifica f = do
  it "e1" $
    gradoExponencial' 2      `shouldBe`  5
  it "e2" $
    gradoExponencial' 25     `shouldBe`  2
  it "e3" $
    gradoExponencial' 15     `shouldBe`  26
  it "p1" $ property $
    \n -> n >= 0 ==> gradoExponencial' n == gradoExponencial2 n
  where gradoExponencial' = f  

  
