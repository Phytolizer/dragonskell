module Main where

import qualified Dragon
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  Dragon.run args
