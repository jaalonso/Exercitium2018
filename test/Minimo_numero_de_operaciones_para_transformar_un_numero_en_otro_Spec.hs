module Minimo_numero_de_operaciones_para_transformar_un_numero_en_otro_Spec (main, spec) where

import Minimo_numero_de_operaciones_para_transformar_un_numero_en_otro
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize) 
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de arbolOp minNOp" $
    verifica arbolOp minNOp

verifica :: (Int -> Int -> Arbol)
            -> (Int -> Int -> Int)
            -> Spec
verifica f g = do
  it "e1" $
    arbolOp' 4 1 `shouldBe` arbolOp 4 1
  it "e2" $
    arbolOp' 2 3 `shouldBe` arbolOp 2 3
  it "e3" $
    arbolOp' 2 4 `shouldBe` arbolOp 2 4
  it "e4" $
    minNOp' 4 7  `shouldBe`  2
  it "e5" $
    minNOp' 2 5  `shouldBe`  4
  modifyMaxSize (const 10) $ it "p1" $ property $
    \x y -> x > 0 && y > 0 ==> minNOp' x y == minNOp x y
  where
    arbolOp' = f
    minNOp'  = g


  
