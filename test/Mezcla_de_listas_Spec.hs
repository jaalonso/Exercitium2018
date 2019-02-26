module Mezcla_de_listas_Spec (main, spec) where

import Mezcla_de_listas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mezcla" $
    verifica mezcla
  describe "Verificacion de mezcla2" $
    verifica mezcla2
  describe "Verificacion de mezcla3" $
    verifica mezcla3
  describe "Verificacion de mezcla4" $
    verifica mezcla4

verifica :: ([[Int]] -> [Int]) -> Spec
verifica f = do
  it "e1" $
    mezcla' [[1,2],[3..7],[8..10]]            `shouldBe`  [1,3,8,2,4,9,5,10,6,7]
  it "e2" $
    take 9 (mezcla' [[3,6..],[5,7..],[0,1]])  `shouldBe`  [3,5,0,6,7,1,9,9,12]
  it "p1" $ property $
    \xss -> mezcla' xss == mezcla xss
  where mezcla' = f

  
