module Example where

import Data.RList
import GMemory

foo = runSIO $ do
  x <- newMemBlock 80
  j <- liftRIO $ mapR (+ 1) x
  y <- readInt j
  return y

bar = runSIO $ do
  x <- newMemBlock 80
  y <- readInt x
  return y
