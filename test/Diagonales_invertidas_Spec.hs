module Diagonales_invertidas_Spec (main, spec) where

import Diagonales_invertidas
import Test.Hspec
import Data.Matrix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de diagonalesInvertidas" $
    verifica diagonalesInvertidas

verifica :: (Matrix Int -> Matrix Int) -> Spec
verifica diagonalesInvertidas' = do
  it "e1" $
    toList (diagonalesInvertidas' (fromList 3 3 [1..])) `shouldBe`
    [9,2,7,4,5,6,3,8,1]
  it "e2" $
    toList (diagonalesInvertidas' (fromList 4 4 [1..])) `shouldBe`
    [16,2,3,13,5,11,10,8,9,7,6,12,4,14,15,1]
  it "e3" $
    toList (diagonalesInvertidas' (fromList 5 5 [1..])) `shouldBe`
    [25,2,3,4,21,6,19,8,17,10,11,12,13,14,15,16,9,18,7,20,5,22,23,24,1]
  
