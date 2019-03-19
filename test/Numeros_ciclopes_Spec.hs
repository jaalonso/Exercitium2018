module Numeros_ciclopes_Spec (main, spec) where

import Numeros_ciclopes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esCiclope ciclopes" $
    verifica esCiclope ciclopes
  describe "Verificacion de esCiclope ciclopes2" $
    verifica esCiclope2 ciclopes2
  describe "Verificacion de esCiclope ciclopes3" $
    verifica esCiclope3 ciclopes3
  describe "Verificacion de esCiclope ciclopes4" $
    verifica esCiclope4 ciclopes4
  describe "Verificacion de esCiclope ciclopes5" $
    verifica esCiclope5 ciclopes5
  describe "Verificacion de esCiclope ciclopes6" $
    verifica esCiclope6 ciclopes6

verifica :: (Integer -> Bool) -> [Integer] -> Spec
verifica esCiclope' ciclopes' = do
  it "e1" $
    esCiclope' 0    `shouldBe`  True
  it "e2" $
    esCiclope' 1    `shouldBe`  False
  it "e3" $
    esCiclope' 5    `shouldBe`  True
  it "e4" $
    esCiclope' 9    `shouldBe`  False
  it "e5" $
    esCiclope' 10   `shouldBe`  False
  it "e6" $
    esCiclope' 27   `shouldBe`  True
  it "e7" $
    esCiclope' 85   `shouldBe`  False
  it "e8" $
    esCiclope' 101  `shouldBe`  False
  it "e9" $
    esCiclope' 111  `shouldBe`  False
  it "e10" $
    esCiclope' 119  `shouldBe`  True
  it "e11" $
    take 6 ciclopes' `shouldBe` [0,5,27,119,495,2015]

  
