module Soluciones_de_x2y3k_Spec (main, spec) where

import Soluciones_de_x2y3k
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de soluciones" $
    verifica soluciones
  describe "Verificacion de soluciones2" $
    verifica soluciones2
  describe "Verificacion de soluciones3" $
    verifica soluciones3

verifica :: [(Integer,Integer,Integer)] -> Spec
verifica f = do
  it "e1" $
    take 6 soluciones' `shouldBe` 
    [(0,0,0),(-1,1,1),(1,1,1),(-8,4,64),(8,4,64),(-27,9,729)]
  it "e2" $
    soluciones' !! (6*10^5+6) `shouldBe` 
    (27000810008100027,90001800009,729043741093514580109350437400729)
  where soluciones' = f
  
