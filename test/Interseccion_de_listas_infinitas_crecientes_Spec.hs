module Interseccion_de_listas_infinitas_crecientes_Spec (main, spec) where

import Interseccion_de_listas_infinitas_crecientes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de interseccion" $
    verifica interseccion
  describe "Verificacion de interseccion2" $
    verifica interseccion2
  describe "Verificacion de interseccion3" $
    verifica interseccion3

verifica :: ([[Int]] -> [Int]) -> Spec
verifica f = do
  it "e1" $
    take 10 (interseccion' [[2,4..],[3,6..],[5,10..]]) `shouldBe` 
      [30,60,90,120,150,180,210,240,270,300]
  it "e2" $
    take 10 (interseccion' [[2,5..],[3,5..],[5,7..]]) `shouldBe` 
      [5,11,17,23,29,35,41,47,53,59]
  where interseccion' = f
    

  
