module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Math (sqrt, pi)

diagonal :: Number → Number → Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number → Number
circleArea r = pi * r * r

main :: ∀ t4. Eff ( console ∷ CONSOLE | t4 ) Unit
main = do
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 5.0)
