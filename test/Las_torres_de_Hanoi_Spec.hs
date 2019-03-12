module Las_torres_de_Hanoi_Spec (main, spec) where

import Las_torres_de_Hanoi
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de movimientos" $
    verifica movimientos

verifica :: (Integer -> [(Integer,Poste,Poste)]) -> Spec
verifica movimientos' = do
  it "e1" $
    movimientos' 1 `shouldBe`
    [(1,A,C)]
  it "e2" $
    movimientos' 2 `shouldBe`
    [(1,A,B),(2,A,C),(1,B,C)]
  it "e3" $
    movimientos' 3 `shouldBe`
    [(1,A,C),(2,A,B),(1,C,B),(3,A,C),(1,B,A),(2,B,C),(1,A,C)]
  it "e4" $
    movimientos 4 `shouldBe`
    [(1,A,B),(2,A,C),(1,B,C),(3,A,B),(1,C,A),(2,C,B),(1,A,B),(4,A,C),
     (1,B,C),(2,B,A),(1,C,A),(3,B,C),(1,A,B),(2,A,C),(1,B,C)]
    

  
