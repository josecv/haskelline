module Hline.Prompt
( Segment(..)
, buildLeftPrompt
) where

import qualified Data.Text as T
import Data.Maybe
import Hline.Icons

data Segment = Segment { text' :: T.Text
                       , fg' :: Maybe String
                       , bg' :: Maybe String
                       } deriving Show

data LeftAcc = LeftAcc T.Text (Maybe String)
             | First

buildLeftPrompt :: [Maybe Segment] -> T.Text
buildLeftPrompt segments =  promptClear `T.append` (renderAcc $ doFold $ catMaybes segments)
    where doFold = foldl folder First
          renderAcc First = "" -- lol empty prompt
          renderAcc (LeftAcc textAcc Nothing) = T.concat [textAcc, bgEnd, fgEnd, " "]
          renderAcc (LeftAcc textAcc lastBg) = T.concat [textAcc, bgEnd, fgFormat lastBg, icon "LEFT_SEGMENT_SEPARATOR", fgEnd, " "]
          folder First segment = LeftAcc (firstSegment segment) (getBg segment)
          folder (LeftAcc textAcc lastBg) (Segment text fg bg) = LeftAcc (T.append textAcc (renderText lastBg (Segment text fg bg))) bg
          renderText Nothing segment = firstSegment segment
          renderText lastBg (Segment text fg bg)
                | lastBg == bg = T.concat [bgFormat bg, fgFormat fg, icon "LEFT_SUBSEGMENT_SEPARATOR", " ", showContent text fg, " "]
                | otherwise = T.concat [bgFormat bg, fgFormat lastBg, icon "LEFT_SEGMENT_SEPARATOR", " ", showContent text fg, " "]
          firstSegment (Segment text fg bg) = T.concat [bgFormat bg,  " ", showContent text fg, " "]

getBg :: Segment -> Maybe String
getBg (Segment _ _ bg) = bg

showContent :: T.Text -> Maybe String -> T.Text
showContent text fg = (fgFormat fg) `T.append` text

bgFormat :: Maybe String -> T.Text
bgFormat Nothing = "%k"
bgFormat (Just bg) = T.concat ["%K{", (T.pack bg), "}"]

fgFormat :: Maybe String -> T.Text
fgFormat Nothing = "%f"
fgFormat (Just fg) = T.concat ["%F{", (T.pack fg), "}"]

bgEnd :: T.Text
bgEnd = "%k"

fgEnd :: T.Text
fgEnd = "%f"

boldEnd :: T.Text
boldEnd = "%b"

promptClear :: T.Text
promptClear = T.concat [bgEnd, fgEnd, boldEnd]
