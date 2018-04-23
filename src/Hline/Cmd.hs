module Hline.Cmd
( Cmd
, resolveCmd
, sh
) where

import System.Process
import System.IO
import qualified Data.Text as T

data CmdHandle = CmdHandle { stdin :: Maybe Handle
                           , stdout :: Maybe Handle
                           , stderr :: Maybe Handle
                           }

data Cmd = Cmd { path :: T.Text } deriving (Show)

sh :: Cmd -> [String] -> IO T.Text
sh (Cmd path) args = (startProc (T.unpack path) args) >>= readHandle
    where startProc cmd args = createProcess (proc cmd args) { std_out = CreatePipe }

resolveCmd :: String -> IO (Maybe Cmd)
resolveCmd cmd = which cmd >>= readHandle >>= isEmpty
    where which cmd = createProcess (shell ("which " ++ cmd)){ std_out = CreatePipe }
          isEmpty "" = return Nothing
          isEmpty s = return (Just (Cmd s))

readHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO T.Text
readHandle (_, Just hout, _, phandle) = (waitForProcess phandle) >>= \_ -> (hGetContents hout >>= \s -> return (T.strip (T.pack s)))
