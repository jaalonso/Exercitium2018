module Reconocimiento_de_conmutatividad_Spec (main, spec) where

import Reconocimiento_de_conmutatividad
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de conmutativa" $
    verifica conmutativa
  describe "Verificacion de conmutativa2" $
    verifica conmutativa2
  describe "Verificacion de conmutativa3" $
    verifica conmutativa3
  describe "Verificacion de conmutativa4" $
    verifica conmutativa4

verifica :: ([[Int]] -> Bool) -> Spec
verifica f = do
  it "e1" $
    conmutativa' [[0,1,2],[1,0,1],[2,1,0]]  `shouldBe`  True
  it "e2" $
    conmutativa' [[0,1,2],[1,0,0],[2,1,0]]  `shouldBe`  False
  it "p1" $ property $
    \(T xss) -> not (null xss) ==>
                conmutativa' xss == conmutativa2 xss
  where conmutativa' = f
    

  
