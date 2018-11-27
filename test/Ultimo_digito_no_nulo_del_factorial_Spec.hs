module Ultimo_digito_no_nulo_del_factorial_Spec (main, spec) where

import Ultimo_digito_no_nulo_del_factorial
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de ultimoNoNuloFactorial" $
    verifica ultimoNoNuloFactorial
  describe "Verificacion de ultimoNoNuloFactorial2" $
    verifica ultimoNoNuloFactorial2

verifica :: (Integer -> Integer) -> Spec
verifica f = do
  it "e1" $
    ultimoNoNuloFactorial  7  `shouldBe` 4
  it "e2" $
    ultimoNoNuloFactorial 10  `shouldBe` 8
  it "e3" $
    ultimoNoNuloFactorial 12  `shouldBe` 6
  it "e4" $
    ultimoNoNuloFactorial 97  `shouldBe` 2
  it "e5" $
    ultimoNoNuloFactorial  0  `shouldBe` 1
  it "p1" $ property $
     prop_ultimoNoNuloFactorial 
  it "p2" $ property $
     prop_equivalencia f
  where ultimoNoNuloFactorial = f

prop_equivalencia :: (Integer -> Integer) -> Integer -> Property
prop_equivalencia f n =
    n >= 0 ==> ultimoNoNuloFactorial2 n == f n
  
