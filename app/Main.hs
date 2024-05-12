module Main (main) where

import EffectfulServer qualified
import MtlServer qualified
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["mtl"] -> MtlServer.run
    _ -> EffectfulServer.run
