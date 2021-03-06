module Listas_equidigitales_Spec (main, spec) where

import Listas_equidigitales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de equidigital" $
    verifica equidigital
  describe "Verificacion de equidigital2" $
    verifica equidigital2
  describe "Verificacion de equidigital3" $
    verifica equidigital3

verifica :: ([Int] -> Bool) -> Spec
verifica f = do
  it "e1" $ 
    equidigital [343,225,777,943]   `shouldBe`  True
  it "e2" $ 
    equidigital [343,225,777,94,3]  `shouldBe`  False
  it "e3" $ property $
    \xss -> equidigital xss == equidigital2 xss
  where equidigital = f
  
