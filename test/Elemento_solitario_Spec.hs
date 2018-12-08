module Elemento_solitario_Spec (main, spec) where

import Elemento_solitario
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de solitario" $
    verifica solitario
  describe "Verificacion de solitario2" $
    verifica solitario2
  describe "Verificacion de solitario3" $
    verifica solitario3
  describe "Verificacion de solitario4" $
    verifica solitario4
  describe "Verificacion de solitario5" $
    verifica solitario5
  describe "Verificacion de solitario6" $
    verifica solitario6
  describe "Verificacion de solitario7" $
    verifica solitario7

verifica :: ([Int] -> Int) -> Spec
verifica f = do
  it "e1" $
    solitario [2,2,7,2]  `shouldBe`  7
  it "e2" $
    solitario [2,2,2,7]  `shouldBe`  7
  it "e3" $
    solitario [7,2,2,2]  `shouldBe`  7
  it "p1" $ property $
    forAll listaSolitaria (\xs -> solitario xs == solitario5 xs)    
  where solitario = f
  
