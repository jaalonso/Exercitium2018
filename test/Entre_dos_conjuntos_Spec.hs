module Entre_dos_conjuntos_Spec (main, spec) where

import Entre_dos_conjuntos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de entreDosConjuntos" $
    verifica entreDosConjuntos
  describe "Verificacion de entreDosConjuntos2" $
    verifica entreDosConjuntos2
  describe "Verificacion de entreDosConjuntos3" $
    verifica entreDosConjuntos3
  describe "Verificacion de entreDosConjuntos4" $
    verifica entreDosConjuntos4
  describe "Verificacion de entreDosConjuntos5" $
    verifica entreDosConjuntos5

verifica :: ([Int] -> [Int] -> [Int]) -> Spec
verifica f = do
  it "e1" $
    entreDosConjuntos' [2,6] [24,36] `shouldBe` [6,12]
  it "e2" $
    entreDosConjuntos' [2,4] [32,16,96] `shouldBe` [4,8,16]
  it "p1" $ property $
    \(L xs) (L ys) -> f xs ys == entreDosConjuntos5 xs ys
  where entreDosConjuntos' = f 
 
