module Mayor_exponente_Spec (main, spec) where

import Mayor_exponente
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mayorExponente" $
    verifica mayorExponente
  describe "Verificacion de mayorExponente2" $
    verifica mayorExponente2
  describe "Verificacion de mayorExponente3" $
    verifica mayorExponente3
  describe "Verificacion de mayorExponente4" $
    verifica mayorExponente4

verifica :: (Integer -> Integer) -> Spec
verifica f = do
  it "e1" $
     mayorExponente' 9   `shouldBe`  2
  it "e2" $
     mayorExponente' 8   `shouldBe`  3
  it "e3" $
     mayorExponente' 7   `shouldBe`  1
  it "e4" $
     mayorExponente' 18  `shouldBe`  1
  it "e5" $
     mayorExponente' 36  `shouldBe`  2
  it "e6" $
     mayorExponente' 144  `shouldBe`  2
  it "p1" $ property $
     \ x -> x > 1 ==> mayorExponente' x == mayorExponente4 x
  where mayorExponente' = f
  
