module Caminos_minimales_en_un_arbol_numerico_Spec (main, spec) where

import Caminos_minimales_en_un_arbol_numerico
import Test.Hspec
import Test.QuickCheck
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mayoresDivisores arbol caminos caminosMinimales " $
    verifica mayoresDivisores arbol caminos caminosMinimales 

verifica :: (Int -> [Int])
            -> (Int -> Tree Int)
            -> (Int -> [[Int]])
            -> (Int -> [[Int]])
            -> Spec
verifica f1 f2 f3 f4 =
  do
    describe "mayoresDivisores" $ do
      it "e1" $
        mayoresDivisores' 24  `shouldBe`  [12,8,6]
      it "e2" $
        mayoresDivisores' 16  `shouldBe`  [8,4]
      it "e3" $
        mayoresDivisores' 10  `shouldBe`  [5]
      it "e4" $
        mayoresDivisores' 17  `shouldBe`  []
      it "prop" $
        property (prop_mayoresDivisores f1)
    describe "arbol" $ do
      it "e1" $
        arbol' 6 == arbol 6
    describe "caminos" $ do
      it "e1" $
        caminos' 6 `shouldBe` [[6,5,4,3,2,1],[6,5,4,2,1],[6,3,2,1]]
      it "e2" $
        caminos' 7 `shouldBe`
          [[7,6,5,4,3,2,1],[7,6,5,4,2,1],[7,6,3,2,1]]
      it "e3" $
        caminos' 8 `shouldBe`
          [[8,7,6,5,4,3,2,1],[8,7,6,5,4,2,1],[8,7,6,3,2,1],[8,4,3,2,1],[8,4,2,1]]
    describe "caminosMinimales" $ do
      it "e1" $
        caminosMinimales' 6 `shouldBe` [[6,3,2,1]]
      it "e2" $
        caminosMinimales' 17 `shouldBe` [[17,16,4,2,1]]
      it "e3" $
        caminosMinimales' 50 `shouldBe`
          [[50,25,5,4,2,1],[50,10,9,3,2,1],[50,10,5,4,2,1]]
      it "e4" $
        caminosMinimales' 7 `shouldBe` [[7,6,3,2,1]]
      it "e5" $
        caminosMinimales' 8 `shouldBe` [[8,4,2,1]]
      it "e6" $
        caminosMinimales' 33 `shouldBe` [[33,32,16,4,2,1],[33,32,8,4,2,1]]
  where mayoresDivisores' = f1
        arbol'            = f2
        caminos'          = f3
        caminosMinimales' = f4

prop_mayoresDivisores :: (Int -> [Int]) -> (Positive Int) -> Bool
prop_mayoresDivisores f (Positive n) =
  mayoresDivisores n == f n

-- prop_arbol :: (Int -> Tree Int) -> Int -> Bool
-- prop_arbol f n =
--   arbol n == f n
--   where m = n `mod` 100

-- prop_caminos :: (Int -> [[Int]]) -> (Positive Int) -> Bool
-- prop_caminos f (Positive n) =
--   caminos n == f n
    

  
