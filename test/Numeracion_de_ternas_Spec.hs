module Numeracion_de_ternas_Spec (main, spec) where

import Numeracion_de_ternas
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize) 
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de posicion" $
    verifica posicion
  describe "Verificacion de posicion2" $
    verifica posicion2
  describe "Verificacion de posicion3" $
    verifica posicion3
  describe "Verificacion de posicion4" $
    verifica posicion4
  describe "Verificacion de posicion5" $
    verifica posicion5

verifica :: ((Int,Int,Int) -> Int) -> Spec
verifica f = do
  it "e1" $
    posicion'(0,1,0)  `shouldBe`  2
  it "e2" $
    posicion' (0,0,2)  `shouldBe`  4
  it "e3" $
    posicion' (0,1,1)  `shouldBe`  5
  modifyMaxSize (const 10) $ it "p1" $ property $
    \x y z -> x >= 0 && y >= 0 && z >= 0 ==>
              posicion' (x,y,z) == posicion5 (x,y,z) 
  where posicion' = f

  
