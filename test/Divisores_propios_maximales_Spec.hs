module Divisores_propios_maximales_Spec (main, spec) where

import Divisores_propios_maximales
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de divisoresPropiosMaximales" $
    verifica divisoresPropiosMaximales nDivisoresPropiosMaximales
  describe "Verificacion de divisoresPropiosMaximales2" $
    verifica divisoresPropiosMaximales2 nDivisoresPropiosMaximales2
  describe "Verificacion de divisoresPropiosMaximales3" $
    verifica divisoresPropiosMaximales2 nDivisoresPropiosMaximales3

verifica :: (Integer -> [Integer])
            -> (Integer -> Integer)
            -> Spec
verifica f g = do
  it "e1" $  
    divisoresPropiosMaximales' 30 `shouldBe`  [6,10,15]
  it "e2" $ 
    divisoresPropiosMaximales' 420 `shouldBe`  [60,84,140,210]
  it "e3" $  
    divisoresPropiosMaximales' 7 `shouldBe`  [1]
  it "e4" $ 
    nDivisoresPropiosMaximales' 30 `shouldBe`  3
  it "e5" $ 
    nDivisoresPropiosMaximales' 420 `shouldBe`  4
  it "e6" $ 
    nDivisoresPropiosMaximales' 7 `shouldBe`  1
  it "p1" $ property $
    \n -> n > 1 ==> divisoresPropiosMaximales' n ==
                    divisoresPropiosMaximales2 n
  it "p2" $ property $
    \n -> n > 1 ==> nDivisoresPropiosMaximales' n ==
                    nDivisoresPropiosMaximales3 n
  where divisoresPropiosMaximales'  = f
        nDivisoresPropiosMaximales' = g
  
