module Exterior_de_arboles_Spec (main, spec) where

import Exterior_de_arboles
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de exterior" $
    verifica exterior

verifica :: (Arbol -> [Int]) -> Spec
verifica f = do
  it "e1" $
    exterior' ejArbol1  `shouldBe`  [3,2,5,1,4,7,6,9,8]
  it "e2" $
    exterior' ejArbol2  `shouldBe`  [3,2,5,7,1,4,9,8]
  it "e3" $
    exterior' ejArbol3  `shouldBe`  [3,2,5,7,6,1,4,9,8]
  where exterior' = f 
