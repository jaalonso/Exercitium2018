module Limites_de_sucesiones_Spec (main, spec) where

import Limites_de_sucesiones
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de limite" $
    verifica limite
  describe "Verificacion de limite2" $
    verifica limite2

verifica :: ([Double] -> Double -> Int -> Double) -> Spec
verifica f = do
  it "e1" $
    limite' [(2*n+1)/(n+5) | n <- [1..]] 0.01 10
     `shouldBe` 1.8953488372093024
  it "e2" $
    limite' [(2*n+1)/(n+5) | n <- [1..]] 0.01 20
     `shouldBe` 1.9262295081967213
  it "e3" $
    limite' [(1+1/n)**n | n <- [1..]] 0.01 10
     `shouldBe` 2.6743187758703026
  where limite' = f
    

  
