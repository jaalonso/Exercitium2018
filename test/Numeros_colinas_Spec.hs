module Numeros_colinas_Spec (main, spec) where

import Numeros_colinas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de esColina" $
    verifica esColina
  describe "Verificacion de esColina2" $
    verifica esColina2

verifica :: (Integer -> Bool) -> Spec
verifica f = do
  it "e1" $
    esColina 12377731  `shouldBe`  True
  it "e2" $
    esColina 1237731   `shouldBe`  True
  it "e3" $
    esColina 123731    `shouldBe`  True
  it "e4" $
    esColina 12377730  `shouldBe`  False
  it "e5" $
    esColina 12377730  `shouldBe`  False
  it "e6" $
    esColina 10377731  `shouldBe`  False
  it "e7" $
    esColina 12377701  `shouldBe`  False
  it "e8" $
    esColina 33333333  `shouldBe`  True
  it "e9" $
    esColina 12374731  `shouldBe`  False
  it "p1" $ property $
    \n -> n >=0 ==> esColina n == esColina2 n 
  where esColina = f
  
