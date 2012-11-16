module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

import Crawl.AccountInfo
import Crawl.Connect
import Crawl.Login
import Crawl.Play

connectAndPlay :: AccountInfo -> IO ()
connectAndPlay config = do
  (sendChan, recvChan) <- connectToWebtiles config
  login sendChan config
  sendRCFile sendChan config
  startGame sendChan config
  play recvChan sendChan

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configFile] -> do
      config <- read <$> readFile configFile
      connectAndPlay config
    _ -> do
      hPutStrLn stderr "usage: rw <acct-file>"
      exitWith (ExitFailure 1)
