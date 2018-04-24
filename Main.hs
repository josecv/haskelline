module Main where

import Hline.Cmd
import Hline.Prompt
import qualified Data.Text as T

main :: IO ()
main = putStrLn $ T.unpack $
                  buildLeftPrompt [Just $ Segment (T.pack "foo") (Just "white") (Just "magenta")
                                 , Nothing
                                 , Just $ Segment (T.pack "fbar") (Just "blue") (Just "green")
                                 , Just $ Segment (T.pack "fbaz") (Just "white") (Just "green")]
