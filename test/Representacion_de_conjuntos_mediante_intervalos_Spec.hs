module Representacion_de_conjuntos_mediante_intervalos_Spec (main, spec) where

import Representacion_de_conjuntos_mediante_intervalos
import Test.Hspec
import Test.QuickCheck
import Data.List (nub)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de intervalos" $
    verifica intervalos

verifica :: ([Int] -> [(Int,Int)]) -> Spec
verifica f = do
  it "e1" $
    intervalos' [2,7,4,3,9,6]  `shouldBe`  [(2,4),(6,7),(9,9)]
  it "e2" $
    intervalos' [180,141,174,143,142,175] `shouldBe` [(141,143),(174,175),(180,180)]
  it "p1" $ property $
    \xs -> not (null xs) ==>
           intervalos' (nub xs) == intervalos (nub xs)
  where intervalos' = f 

  
