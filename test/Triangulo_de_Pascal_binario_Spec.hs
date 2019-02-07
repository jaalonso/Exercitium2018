module Triangulo_de_Pascal_binario_Spec (main, spec) where

import Triangulo_de_Pascal_binario
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de trianguloPascalBinario pascalCapicuas nPascalCapicuas" $
    verifica trianguloPascalBinario pascalCapicuas nPascalCapicuas
  describe "Verificacion de trianguloPascalBinario pascalCapicuas nPascalCapicuas2" $
    verifica trianguloPascalBinario2 pascalCapicuas nPascalCapicuas2

verifica :: ([Int] -> [[Int]])
         -> (Int -> [[Int]])
         -> (Int -> Integer)
         -> Spec
verifica f g h = do
  it "e1" $
    trianguloPascalBinario' [1,0,1,1,1]
      `shouldMatchList` [[1,0,1,1,1],[1,1,0,0],[0,1,0],[1,1],[0]]
  it "e2" $    
    trianguloPascalBinario' [1,0,1,1,0]
      `shouldMatchList` [[1,0,1,1,0],[1,1,0,1],[0,1,1],[1,0],[1]]
  it "e3" $    
    pascalCapicuas' 2
      `shouldMatchList` [[0,0],[1,0]]
  it "e4" $      
    pascalCapicuas' 3
      `shouldMatchList` [[0,0,0],[0,1,0],[1,0,0],[1,1,0]]
  it "e5" $      
    pascalCapicuas' 4
      `shouldMatchList` [[0,0,0,0],[0,1,1,0],[1,0,0,0],[1,1,1,0]]
  it "e6" $      
    nPascalCapicuas' 2
      `shouldBe` 2
  it "e7" $      
    nPascalCapicuas' 3
      `shouldBe` 4
  it "e8" $      
    nPascalCapicuas' 4
      `shouldBe` 4
  where
    trianguloPascalBinario' = f 
    pascalCapicuas'         = g
    nPascalCapicuas'        = h 
  
    
