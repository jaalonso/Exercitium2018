module Ofertas_3_por_2_Spec (main, spec) where

import Ofertas_3_por_2
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de minimoConOferta" $
    verifica minimoConOferta
  describe "Verificacion de minimoConOferta2" $
    verifica minimoConOferta2
  describe "Verificacion de minimoConOferta3" $
    verifica minimoConOferta3
  describe "Verificacion de minimoConOferta4" $
    verifica minimoConOferta4
  describe "Verificacion de minimoConOferta5" $
    verifica minimoConOferta5

verifica :: ([Int] -> Int) -> Spec
verifica f = do
  it "e1" $
    minimoConOferta' [10,2,4,5]     `shouldBe`  17
  it "e2" $
    minimoConOferta' [3,2,3,2]      `shouldBe`  8
  it "e3" $
    minimoConOferta' [6,4,5,5,5,5]  `shouldBe`  21
  it "p1" $ property $
    \xs -> let ys = map abs xs
           in minimoConOferta' ys == minimoConOferta ys
  where minimoConOferta' = f

  
