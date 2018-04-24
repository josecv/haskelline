module Hline.Icons
( icon
)
where

import qualified Data.Text as T

icon :: String -> T.Text
icon "LEFT_SUBSEGMENT_SEPARATOR" = "\xE0B1"
icon "LEFT_SEGMENT_SEPARATOR" = "\xE0B0"
icon _ = error "No such icon"
