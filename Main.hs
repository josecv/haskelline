module Main where

import Hline.Prompt
import Hline.Segments
import Hline.SegmentExecutor
import qualified Data.Text as T

main :: IO ()
main = (fmap (\x -> buildLeftPrompt [x]) $ execute KubeControl) >>= (putStrLn . T.unpack)
