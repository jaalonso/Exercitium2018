module Numeros_de_parejas_Spec (main, spec) where

import Numeros_de_parejas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nParejas" $
    verifica nParejas
  describe "Verificacion de nParejas2" $
    verifica nParejas2
  describe "Verificacion de nParejas3" $
    verifica nParejas3
  describe "Verificacion de nParejas4" $
    verifica nParejas4

verifica :: ([Int] -> Int) -> Spec
verifica f = do
  it "e1" $
    nParejas [1,2,2,1,1,3,5,1,2]  `shouldBe`  3
  it "e2" $
    nParejas [1,2,1,2,1,3,2]      `shouldBe`  2
  it "p1" $ property $
    \xs -> nParejas xs == nParejas2 xs
  where nParejas = f
  
