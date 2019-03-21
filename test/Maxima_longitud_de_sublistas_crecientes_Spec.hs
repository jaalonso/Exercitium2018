module Maxima_longitud_de_sublistas_crecientes_Spec (main, spec) where

import Maxima_longitud_de_sublistas_crecientes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de longitudMayorSublistaCreciente" $
    verifica longitudMayorSublistaCreciente1
  describe "Verificacion de longitudMayorSublistaCreciente2" $
    verifica longitudMayorSublistaCreciente2
  describe "Verificacion de longitudMayorSublistaCreciente3" $
    verifica longitudMayorSublistaCreciente3

verifica :: ([Int] -> Int) -> Spec
verifica longitudMayorSublistaCreciente' = do
  it "e1" $
    longitudMayorSublistaCreciente' [3,2,6,4,5,1] `shouldBe`
    3
  it "e2" $
    longitudMayorSublistaCreciente' [10,22,9,33,21,50,41,60,80] `shouldBe`
    6
  it "e3" $
    longitudMayorSublistaCreciente' [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]
    `shouldBe`
    6
  
