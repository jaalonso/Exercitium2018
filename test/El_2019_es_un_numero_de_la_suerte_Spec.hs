module El_2019_es_un_numero_de_la_suerte_Spec (main, spec) where

import El_2019_es_un_numero_de_la_suerte
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de numerosDeLaSuerte esNumeroDeLaSuerte" $
    verifica numerosDeLaSuerte esNumeroDeLaSuerte

verifica :: [Int] -> (Int -> Bool) -> Spec
verifica f g = do
  it "e1" $
    take 20 numerosDeLaSuerte' `shouldBe`
     [1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
  it "e2" $
   numerosDeLaSuerte' !! 277 `shouldBe`  2019
  it "e3" $
   esNumeroDeLaSuerte' 15    `shouldBe`  True
  it "e4" $
   esNumeroDeLaSuerte' 16    `shouldBe`  False
  it "e5" $
   esNumeroDeLaSuerte' 2019  `shouldBe`  True
  where
   numerosDeLaSuerte'  = f 
   esNumeroDeLaSuerte' = g
  
   
