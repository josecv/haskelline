module Hline.Segments
( KubeControl(..)

)
where

import Hline.Cmd
import Hline.SegmentExecutor
import Hline.Prompt
-- import Control.Parallel

data KubeControl = KubeControl

instance SegmentExecutor KubeControl where
    execute KubeControl = (resolveCmd "kubectl") >>= buildSegment
        where buildSegment Nothing = return Nothing
              buildSegment (Just cmd) = getText cmd >>= return . Just . (\x -> Segment x (Just "white") (Just "magenta"))
              getText cmd = sh cmd ["version"]
