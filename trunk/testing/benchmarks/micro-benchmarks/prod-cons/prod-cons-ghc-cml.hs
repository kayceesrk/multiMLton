{-# Language ScopedTypeVariables #-}
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.CML

send c v = sync $ transmit c v
recv c = sync $ receive c (\_ -> True)

main = do
  args <- getArgs
  let numMsgs::Int = read $ head args
  c <- channel
  dc <- channel
  let producer = do
        replicateM_ numMsgs $ send c 1
        send dc "DONE"
  let consumer = do
        replicateM_ numMsgs $ recv c
        send dc "DONE"
  forkIO producer
  forkIO consumer
  recv dc
  recv dc
