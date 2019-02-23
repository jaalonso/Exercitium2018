module Posiciones_del_2019_en_el_numero_pi_Spec (main, spec) where

import Posiciones_del_2019_en_el_numero_pi
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de posiciones" $
    verifica posiciones
  describe "Verificacion de posiciones2" $
    verifica posiciones2

verifica :: (String -> Int -> IO [Int]) -> Spec
verifica f = do
  it "e1" $ do
    ds <- posiciones' "2019" 1000
    ds `shouldBe` [243,701,994]
  it "e2" $ do
    posiciones' "2018" 10000  >>= (`shouldBe` [4067])
  it "e3" $ do
    posiciones' "141" 1000  >>= (`shouldBe` [0,294])
  it "e4" $ do
    posiciones' "4159" 10000 >>= (`shouldBe` [1,5797,6955,9599])
  where posiciones' = f

  
