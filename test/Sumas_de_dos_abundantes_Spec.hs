module Sumas_de_dos_abundantes_Spec (main, spec) where

import Sumas_de_dos_abundantes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de sumasDeDosAbundantes" $
    verifica sumasDeDosAbundantes

verifica :: [Integer] -> Spec
verifica f = do
  it "e1" $
    take 10 sumasDeDosAbundantes'  `shouldBe`
    [24,30,32,36,38,40,42,44,48,50]
  where sumasDeDosAbundantes' = f 
  
