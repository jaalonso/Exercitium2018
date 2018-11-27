module Capicuas_productos_de_dos_numeros_de_dos_digitos_Spec (main, spec) where

import Capicuas_productos_de_dos_numeros_de_dos_digitos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de capicuasP2N2D" $
    verifica capicuasP2N2D

verifica :: [Int] -> Spec
verifica f = do
  it "e1" $
    take 5  capicuasP2N2D  ==  [121,242,252,272,323] 
  it "e2" $
    length  capicuasP2N2D  ==  74                    
  it "e3" $
    drop 70 capicuasP2N2D  ==  [8008,8118,8448,9009]
  where capicuasP2N2D = f
  
