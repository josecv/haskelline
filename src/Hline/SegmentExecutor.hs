module Hline.SegmentExecutor
( SegmentExecutor(..)

) where

import Hline.Prompt

class SegmentExecutor a where
    execute :: a -> IO (Maybe Segment)
