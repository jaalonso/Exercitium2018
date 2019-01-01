module Numero_de_divisores_compuestos_Spec (main, spec) where

import Numero_de_divisores_compuestos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de nDivisoresCompuestos" $
    verifica nDivisoresCompuestos
  describe "Verificacion de nDivisoresCompuestos2" $
    verifica nDivisoresCompuestos2
  describe "Verificacion de nDivisoresCompuestos3" $
    verifica nDivisoresCompuestos3

verifica :: (Integer -> Integer) -> Spec
verifica f = do
  it "e1" $
    nDivisoresCompuestos' 30  `shouldBe`  4
  it "p1" $ property $
    \n -> n > 0 ==> nDivisoresCompuestos' n == nDivisoresCompuestos2 n
  where nDivisoresCompuestos' = f

  
