{-# Language ScopedTypeVariables #-}
import Control.Concurrent
import System.Environment
import Control.Monad

main = do
  args <- getArgs
  let numThreads::Int = read $ head args
  replicateM_ numThreads $ forkIO $ return ()
