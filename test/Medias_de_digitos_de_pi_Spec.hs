module Medias_de_digitos_de_pi_Spec (main, spec) where

import Medias_de_digitos_de_pi
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de mediasDigitosDePi" $
    verifica mediasDigitosDePi

verifica :: IO [Double] -> Spec
verifica f = do
  it "e1" $ do
    xs <- mediasDigitosDePi
    take 100 <$> mediasDigitosDePi' `shouldReturn` take 100 xs
  where mediasDigitosDePi' = f

  
