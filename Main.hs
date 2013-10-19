{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr,
                  openFile, IOMode(WriteMode), hFlush)

import Control.Concurrent.Chan.Split (OutChan, dupChan, readChan)
import System.Remote.Monitoring (forkServer)
import Text.Groom (groom)

import Crawl.AccountInfo
import Crawl.Connect
import Crawl.Login
import Crawl.Play

logReceived :: Show a => OutChan a -> IO ()
logReceived recvChan = do
  recvLogged <- dupChan recvChan
  forkIO $ do
    logFile <- openFile "rw.log" WriteMode
    forever $ do
      item <- readChan recvLogged
      hPutStrLn logFile $ groom item
      hFlush logFile
  return ()

connectAndPlay :: AccountInfo -> IO ()
connectAndPlay config = do
  (sendChan, recvChan) <- connectToWebtiles config
  logReceived recvChan
  login sendChan config
  sendRCFile sendChan config
  startGame sendChan config
  play recvChan sendChan

main :: IO ()
main = do
  forkServer "localhost" 8000
  args <- getArgs
  case args of
    [configFile] -> do
      config <- read <$> readFile configFile
      connectAndPlay config
    _ -> do
      hPutStrLn stderr "usage: rw <acct-file>"
      exitWith (ExitFailure 1)
