{-# Language ScopedTypeVariables #-}
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan

main = do
  args <- getArgs
  let numMsgs::Int = read $ head args
  c <- newChan
  dc <- newChan
  let producer = do
        replicateM_ numMsgs $ writeChan c 1
        writeChan dc "DONE"
  let consumer = do
        replicateM_ numMsgs $ readChan c
        writeChan dc "DONE"
  forkIO producer
  forkIO consumer
  readChan dc
  readChan dc
