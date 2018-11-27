module Numeros_libres_de_cuadrados_Spec (main, spec) where

import Numeros_libres_de_cuadrados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de libreDeCuadrados" $
    verifica libreDeCuadrados
  describe "Verificacion de libreDeCuadrados2" $
    verifica libreDeCuadrados2
  describe "Verificacion de libreDeCuadrados3" $
    verifica libreDeCuadrados3
  describe "Verificacion de libreDeCuadrados4" $
    verifica libreDeCuadrados4

verifica :: (Integer -> Bool) -> Spec
verifica f = do
  it "e1" $
    libreDeCuadrados 70     `shouldBe`  True  
  it "e2" $
    libreDeCuadrados 40     `shouldBe`  False 
  it "p1" $ property $
    \x -> x > 0 ==> libreDeCuadrados x == libreDeCuadrados3 x
  where libreDeCuadrados = f
  
