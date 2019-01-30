module Sucesion_triangular_Spec (main, spec) where

import Sucesion_triangular
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de terminoSucTriangular" $
    verifica terminoSucTriangular
  describe "Verificacion de terminoSucTriangular2" $
    verifica terminoSucTriangular2
  describe "Verificacion de terminoSucTriangular3" $
    verifica terminoSucTriangular3

verifica :: (Int -> Integer) -> Spec
verifica f = do
  it "e1" $ 
    terminoSucTriangular' 5     `shouldBe`  3
  it "e2" $ 
    terminoSucTriangular' 10    `shouldBe`  1
  it "e3" $ 
    terminoSucTriangular' 20    `shouldBe`  6
  it "e4" $ 
    terminoSucTriangular' 100   `shouldBe`  10
  it "e5" $ 
    terminoSucTriangular' 1001  `shouldBe`  12
  where terminoSucTriangular' = f

  
