module Crawl.Connect (
  connectToWebtiles
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent.Chan.Split (InChan, OutChan, newSplitChan,
                                      readChan, writeChan)
import Network.WebSockets (connect, WebSockets, Hybi10,
                           getSink, sendSink,
                           receiveDataMessage,
                           Message(DataMessage),
                           DataMessage(Text, Binary))
import Codec.Zlib (initInflate, Inflate, WindowBits(WindowBits),
                   feedInflate, flushInflate)
import qualified Data.Aeson as A

import Crawl.AccountInfo

decompress :: Inflate -> BL.ByteString -> IO BL.ByteString
decompress inflator compressed = do
  -- XXX use lazy ByteString input in a sane way
  popper <- feedInflate inflator (B.concat (BL.toChunks compressed ++ [B.pack [0,0,255,255]]))
  let pop = do
        maybePiece <- popper
        case maybePiece of
          Nothing -> (:[]) <$> flushInflate inflator
          Just piece -> (piece:) <$> pop
  BL.fromChunks <$> pop

-- XXX automatically reply to pings?
connectToWebtiles :: AccountInfo -> IO (InChan A.Object, OutChan A.Object)
connectToWebtiles config = do
  (sendIncoming, recvIncoming) <- newSplitChan
  (sendOutgoing, recvOutgoing) <- newSplitChan
  forkIO $ connect (server config) 8080 "/socket" $ asHybi10 $ do
    sink <- getSink
    liftIO $ forkIO $ forever $ do
      object <- readChan recvOutgoing
      sendSink sink $ DataMessage (Text (A.encode object))
    inflator <- liftIO $ initInflate (WindowBits (-15)) -- raw zlib
    forever $ do
      Binary compressed <- receiveDataMessage
      liftIO $ do
        decompressed <- decompress inflator compressed
        writeChan sendIncoming $
          fromMaybe (error "JSON decoding error") (A.decode decompressed)
  return (sendOutgoing, recvIncoming)
  where asHybi10 :: WebSockets Hybi10 a -> WebSockets Hybi10 a
        asHybi10 = id
