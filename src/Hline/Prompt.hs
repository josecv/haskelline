module Hline.Prompt
( Segment(..)
, buildLeftPrompt
, buildRightPrompt
) where

import qualified Data.Text as T
import Data.Maybe
import Hline.Icons

data Segment = Segment { text :: T.Text
                       , fg :: Maybe String
                       , bg :: Maybe String
                       } deriving Show

data SegmentAcc = SegmentAcc T.Text (Maybe String)
                           | First

buildLeftPrompt :: [Maybe Segment] -> T.Text
buildLeftPrompt = buildPrompt renderAcc renderText
    where renderAcc First = "" -- lol empty prompt
          renderAcc (SegmentAcc textAcc Nothing) = T.concat [promptClear, textAcc, bgEnd, fgEnd, " "]
          renderAcc (SegmentAcc textAcc lastBg) = T.concat [promptClear, textAcc, bgEnd, fgFormat lastBg, icon "LEFT_SEGMENT_SEPARATOR", fgEnd, " "]
          renderText Nothing (Segment text' fg' bg') = T.concat [bgFormat bg',  " ", showContent text' fg', " "]
          renderText lastBg (Segment text' fg' bg')
                | lastBg == bg' = T.concat [fgFormat fg', icon "LEFT_SUBSEGMENT_SEPARATOR", " ", text', " "]
                | otherwise = T.concat [bgFormat bg', fgFormat lastBg, icon "LEFT_SEGMENT_SEPARATOR", " ", showContent text' fg', " "]

buildRightPrompt :: [Maybe Segment] -> T.Text
buildRightPrompt = buildPrompt renderAcc renderText
    where renderAcc First = ""
          renderAcc (SegmentAcc textAcc _) = T.concat [promptClear, textAcc, promptClear]
          renderText Nothing (Segment text' fg' bg') = T.concat [fgFormat bg', icon "RIGHT_SEGMENT_SEPARATOR", bgFormat bg', " ", showContent text' fg', " "]
          renderText lastBg (Segment text' fg' bg')
                | lastBg == bg' = T.concat [fgFormat fg', icon "RIGHT_SUBSEGMENT_SEPARATOR", " ", text', " "]
                | otherwise = T.concat [fgFormat bg', icon "RIGHT_SEGMENT_SEPARATOR", bgFormat bg', " ", showContent text' fg', " "]

buildPrompt :: (SegmentAcc -> T.Text) -> (Maybe String -> Segment -> T.Text) -> [Maybe Segment] -> T.Text
buildPrompt renderAcc renderText = renderAcc . foldl folder First . catMaybes
    where folder First segment = SegmentAcc (renderText Nothing segment) (getBg segment)
          folder (SegmentAcc textAcc lastBg) (Segment text' fg' bg') = SegmentAcc (T.append textAcc $ renderText lastBg $ Segment text' fg' bg') bg'


getBg :: Segment -> Maybe String
getBg (Segment _ _ bg') = bg'

showContent :: T.Text -> Maybe String -> T.Text
showContent text' fg' = (fgFormat fg') `T.append` text'

bgFormat :: Maybe String -> T.Text
bgFormat Nothing = bgEnd
bgFormat (Just bg') = T.concat ["%K{", T.pack bg', "}"]

fgFormat :: Maybe String -> T.Text
fgFormat Nothing = fgEnd
fgFormat (Just fg') = T.concat ["%F{", T.pack fg', "}"]

bgEnd :: T.Text
bgEnd = "%k"

fgEnd :: T.Text
fgEnd = "%f"

boldEnd :: T.Text
boldEnd = "%b"

promptClear :: T.Text
promptClear = T.concat [bgEnd, fgEnd, boldEnd]
