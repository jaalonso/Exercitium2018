module Subarboles_monovalorados_Spec (main, spec) where

import Subarboles_monovalorados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de monovalorados" $
    verifica monovalorados

verifica :: (Arbol -> [Arbol]) -> Spec
verifica f = do
  it "e1" $
    monovalorados' (N 5 (H 5) (H 5))
    `shouldBe` [N 5 (H 5) (H 5),H 5,H 5] 
  it "e2" $
    monovalorados' (N 5 (H 5) (H 6))
    `shouldBe` [H 5,H 6] 
  it "e3" $
    monovalorados' (N 9 (H 9) (N 9 (H 9) (H 9)))
    `shouldBe` [N 9 (H 9) (N 9 (H 9) (H 9)),H 9,N 9 (H 9) (H 9),H 9,H 9] 
  it "e4" $
    monovalorados' (N 9 (H 9) (N 7 (H 9) (H 9)))
    `shouldBe` [H 9,H 9,H 9] 
  it "e5" $
    monovalorados' (N 9 (H 9) (N 9 (H 7) (H 9)))
    `shouldBe` [H 9,H 7,H 9] 
  where monovalorados' = f
  
