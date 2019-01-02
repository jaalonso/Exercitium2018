module Tablas_de_operaciones_binarias_Spec (main, spec) where

import Tablas_de_operaciones_binarias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de tablaOperacion tablaSuma tablaResta tablaProducto" $
    verifica tablaOperacion tablaSuma tablaResta tablaProducto
  
verifica :: ((Int -> Int -> Int) -> Int -> [[Int]])
            -> (Int -> [[Int]])
            -> (Int -> [[Int]])
            -> (Int -> [[Int]])
            -> Spec
verifica f1 f2 f3 f4 = do
  it "e1" $
    tablaOperacion' (+) 3  `shouldBe`  [[0,1,2],[1,2,0],[2,0,1]]
  it "e2" $
    tablaOperacion' (-) 3  `shouldBe`  [[0,2,1],[1,0,2],[2,1,0]]
  it "e3" $
    tablaOperacion' (-) 4  `shouldBe`  [[0,3,2,1],[1,0,3,2],[2,1,0,3],[3,2,1,0]]
  it "e4" $
    tablaOperacion' (\x y -> abs (x-y)) 3  `shouldBe`  [[0,1,2],[1,0,1],[2,1,0]]
  it "e5" $
    tablaSuma' 3  `shouldBe`  [[0,1,2],[1,2,0],[2,0,1]]
  it "e6" $
    tablaSuma' 4  `shouldBe`  [[0,1,2,3],[1,2,3,0],[2,3,0,1],[3,0,1,2]]
  it "e7" $
    tablaResta' 3  `shouldBe`  [[0,2,1],[1,0,2],[2,1,0]]
  it "e8" $
    tablaResta' 4  `shouldBe`  [[0,3,2,1],[1,0,3,2],[2,1,0,3],[3,2,1,0]]
  it "e9" $
    tablaProducto' 3  `shouldBe`  [[0,0,0],[0,1,2],[0,2,1]]
  it "e10" $
    tablaProducto' 4  `shouldBe`  [[0,0,0,0],[0,1,2,3],[0,2,0,2],[0,3,2,1]]
  where
    tablaOperacion' = f1
    tablaSuma'      = f2
    tablaResta'     = f3
    tablaProducto'  = f4
