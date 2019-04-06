module Subconjuntos_divisibles_Spec (main, spec) where

import Subconjuntos_divisibles
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Verificacion de subconjuntosDivisibles" $
    verifica subconjuntosDivisibles
  describe "Verificacion de subconjuntosDivisibles2" $
    verifica subconjuntosDivisibles2
  describe "Verificacion de subconjuntosDivisibles3" $
    verifica subconjuntosDivisibles3

verifica :: ([Int] -> [[Int]]) -> Spec
verifica subconjuntosDivisibles' = do
  it "e1" $
    subconjuntosDivisibles' []         `shouldMatchList`  [[]]
  it "e2" $
    subconjuntosDivisibles' [1]        `shouldMatchList`  [[]] 
  it "e3" $
    subconjuntosDivisibles' [3]        `shouldMatchList`  [[3],[]]
  it "e4" $
    subconjuntosDivisibles' [1,3]      `shouldMatchList`  [[3],[]]
  it "e5" $
    subconjuntosDivisibles' [3,6]      `shouldMatchList`  [[3,6],[3],[6],[]]
  it "e6" $
    subconjuntosDivisibles' [1,3,6]    `shouldMatchList`  [[3,6],[3],[6],[]]
  it "e7" $
    subconjuntosDivisibles' [2,3,6]    `shouldMatchList`  [[2,6],[2],[3,6],[3],[6],[]]
  it "e8" $
    subconjuntosDivisibles' [2,3,6,8]  `shouldMatchList`
      [[2,6,8],[2,6],[2,8],[2],[3,6],[3],[6,8],[6],[8],[]]
  
