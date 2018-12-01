module Numeros_autodescriptivos_Spec (main, spec) where

import Numeros_autodescriptivos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de autodescriptivo" $
    verifica autodescriptivo

verifica :: (Integer -> Bool) -> Spec
verifica f = do
  it "e1" $
    autodescriptivo 1210 `shouldBe` True 
  it "e2" $
    [x | x <- [1..100000], autodescriptivo x] `shouldBe` [1210,2020,21200]
  where autodescriptivo = f
