module Main where

import Hline.Prompt
import Hline.Segments
import qualified Data.Text as T
import Control.Parallel.Strategies

main :: IO ()
main = (sequence $ parMap rpar runSegment ["kubecontext"]) >>= (putStrLn . T.unpack . buildLeftPrompt)
