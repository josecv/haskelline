module Hline.Cmd
( Cmd
, resolveCmd
, sh
) where

import System.Process
import System.IO
import qualified Data.Text as T

data Cmd = Cmd { path :: T.Text } deriving (Show)

sh :: Cmd -> [String] -> IO T.Text
sh (Cmd cmd) args = (startProc (T.unpack cmd) args) >>= readHandle
    where startProc cmd' args' = createProcess (proc cmd' args') { std_out = CreatePipe }

resolveCmd :: String -> IO (Maybe Cmd)
resolveCmd cmd = fmap isEmpty $ which cmd >>= readHandle
    where which cmd' = createProcess (shell ("which " ++ cmd')){ std_out = CreatePipe }
          isEmpty "" = Nothing
          isEmpty s = (Just (Cmd s))

readHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO T.Text
readHandle (_, Just hout, _, phandle) = (waitForProcess phandle) >>= \_ -> (hGetContents hout >>= \s -> return (T.strip (T.pack s)))
readHandle (_, Nothing, _, _) = error "Can't read when there's no stdout"
