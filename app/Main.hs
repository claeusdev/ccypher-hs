module Main (main) where

import Caesar

main :: IO ()
main = do
  print $ encode 3 "Are we safe"
  print $ decode 3 (encode 3 "Are we safe")
  print $ frequencyTable "the blue white fox jumps over the lazy dog"
