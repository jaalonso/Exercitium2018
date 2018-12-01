module Ceros_finales_del_factorial_Spec (main, spec) where

import Ceros_finales_del_factorial
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de cerosDelFactorial" $
    verifica cerosDelFactorial
  describe "Verificacion de cerosDelFactorial2" $
    verifica cerosDelFactorial2
  describe "Verificacion de cerosDelFactorial3" $
    verifica cerosDelFactorial3

verifica :: (Integer -> Integer) -> Spec
verifica f = do
  it "e1" $
    cerosDelFactorial 24 `shouldBe`  4 
  it "e2" $
    cerosDelFactorial 25 `shouldBe`  6
  it "p1" $ property $
    \x -> x >= 0 ==> cerosDelFactorial x == cerosDelFactorial3 x
  where cerosDelFactorial = f

  
