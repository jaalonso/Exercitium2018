module Aritmetica_lunar_Spec (main, spec) where

import Aritmetica_lunar
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de suma producto" $
    verifica suma producto

verifica :: (Integer -> Integer -> Integer)
         -> (Integer -> Integer -> Integer)
         -> Spec
verifica f g = do
  it "e1" $  
    suma' 357 64  `shouldBe`  367
  it "e2" $  
    suma' 64 357  `shouldBe`  367
  it "e3" $  
    suma' 1 3     `shouldBe`  3
  it "e4" $  
    suma' 7 4     `shouldBe`  7
  it "e5" $  
    producto' 357 64  `shouldBe`  3564
  it "e6" $  
    producto' 64 357  `shouldBe`  3564
  it "e7" $  
    producto' 1 3     `shouldBe`  1
  it "e8" $  
    producto' 7 4     `shouldBe`  4
  it "p1" $ property $
    \x y -> x >= 0 && y >= 0 ==> suma' x y == suma x y
  it "p2" $ property $
    \x y -> x >= 0 && y >= 0 ==> producto' x y == producto x y
  where suma'     = f
        producto' = g

 
  
