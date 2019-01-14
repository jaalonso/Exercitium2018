module Cadena_descendiente_de_subnumeros_Spec (main, spec) where

import Cadena_descendiente_de_subnumeros
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de cadena" $
    verifica cadena
  describe "Verificacion de cadena2" $
    verifica cadena2

verifica :: (Integer -> [Integer]) -> Spec
verifica f = do
  it "e1" $
    cadena' 2019         `shouldBe` [20,19]           
  it "e2" $
    cadena' 201200199198 `shouldBe` [201,200,199,198] 
  it "e3" $
    cadena' 3246         `shouldBe` [3246]            
  it "e4" $
    cadena' 87654        `shouldBe` [8,7,6,5,4]       
  it "e5" $
    cadena' 123456       `shouldBe` [123456]          
  it "e6" $
    cadena' 1009998      `shouldBe` [100,99,98]       
  it "e7" $
    cadena' 100908       `shouldBe` [100908]          
  it "e8" $
    cadena' 1110987      `shouldBe` [11,10,9,8,7]     
  it "e9" $
    cadena' 210          `shouldBe` [2,1,0]           
  it "e10" $
    cadena' 1            `shouldBe` [1]               
  it "e11" $
    cadena' 0            `shouldBe` [0]               
  it "e12" $
    cadena' 312          `shouldBe` [312]             
  it "e13" $
    cadena' 191          `shouldBe` [191]
  where cadena' = f
  
