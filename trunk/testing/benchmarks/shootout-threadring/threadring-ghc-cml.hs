-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
-- Contributed by Jed Brown with improvements by Spencer Janssen, Don Stewart and Alex Mason
--
-- Compile with: <ghc> --make -O2 -fglasgow-exts -threaded threadring.ghc-4.hs -o threadring.ghc-4.ghc_run

import Control.Monad
import Control.Concurrent (forkOn)
import Control.Concurrent.CML
import System.Environment
import GHC.Conc

ring = 503
send c v = sync $ transmit c v
recv c = sync $ receive c (\_ -> True)

new ret l i = do
  r <- channel
  forkOn numCapabilities (thread ret i l r)
  return r


thread :: Channel () -> Int -> Channel Int -> Channel Int -> IO ()
thread ret i l r = go
  where go = do
          m <- recv l
          if m > 1
              then (send r $! m - 1) >> go
              else print i >> send ret ()

main = do
  n <- return . read . head =<< getArgs
  ret <- channel
  a <- channel
  z <- foldM (new ret) a [2..ring]
  forkOn numCapabilities (thread ret 1 z a)
  send z (n+1)
  recv ret
