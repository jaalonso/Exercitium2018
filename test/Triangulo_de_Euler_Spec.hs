module Triangulo_de_Euler_Spec (main, spec) where

import Triangulo_de_Euler
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de trianguloEuler" $
    verifica trianguloEuler
  describe "Verificacion de trianguloEuler2" $
    verifica trianguloEuler2
  describe "Verificacion de trianguloEuler3" $
    verifica trianguloEuler3

verifica :: [[Integer]] -> Spec
verifica trianguloEuler' = do
  it "e1" $
    take 6 trianguloEuler' `shouldBe`
    [[1],[1,1],[1,4,1],[1,11,11,1],[1,26,66,26,1],[1,57,302,302,57,1]]
  
