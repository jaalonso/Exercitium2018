module Divisores_compuestos_Spec (main, spec) where

import Divisores_compuestos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de divisoresCompuestos" $
    verifica divisoresCompuestos
  describe "Verificacion de divisoresCompuestos2" $
    verifica divisoresCompuestos2
  describe "Verificacion de divisoresCompuestos3" $
    verifica divisoresCompuestos3
  describe "Verificacion de divisoresCompuestos4" $
    verifica divisoresCompuestos4
  describe "Verificacion de divisoresCompuestos5" $
    verifica divisoresCompuestos5
  describe "Verificacion de divisoresCompuestos6" $
    verifica divisoresCompuestos6

verifica :: (Integer -> [Integer]) -> Spec
verifica f = do
  it "e1" $ 
    divisoresCompuestos' 30  `shouldBe`  [6,10,15,30]
  it "p1" $ property $
    \x -> x > 0 ==> divisoresCompuestos' x ==
                    divisoresCompuestos6 x
  where divisoresCompuestos' = f

  
