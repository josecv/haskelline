module Hline.Segments
( runSegment

)
where

import Hline.Cmd
import Hline.SegmentExecutor
import Hline.Prompt
import Control.Parallel
import qualified Data.Text as T

runSegment :: T.Text -> IO (Maybe Segment)
runSegment "kubecontext" = execute KubeContext
runSegment _ = error "No such segment"

data KubeContext = KubeContext

instance SegmentExecutor KubeContext where
    execute KubeContext = (resolveCmd "kubectl") >>= buildSegment
        where buildSegment Nothing = return Nothing
              buildSegment (Just cmd) = getText cmd >>= return . Just . (\x -> Segment x (Just "white") (Just "magenta"))
              getText cmd = let namespace = (\c -> T.words ([t | t <- (T.lines c), (T.head t) == '*'] !! 0) !! 4) <$> (sh cmd ["config", "get-contexts", "--no-headers"])
                                context = sh cmd ["config", "current-context"]
                            in  par namespace $ pseq context $ (\x y -> if x /= y then T.concat [x, "/", y] else x) <$> context <*> namespace
