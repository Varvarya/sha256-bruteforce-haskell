module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

threadLoop :: Int -> TVar Int -> TVar [(Int, Int)] -> IO ()
threadLoop num counter printQueue = do
  cnt <- atomically $ do
           t <- readTVar counter
           let t' = t - 1
           writeTVar counter t'
           return t'
  unless (cnt < 0) $ do
    atomically $ modifyTVar' printQueue (\ q -> (num, cnt) : q)
    threadLoop num counter printQueue

printLoop :: TVar [(Int,Int)] -> IO ()
printLoop printQueue = do
  q <- atomically $ swapTVar printQueue []
  case q of
    [] ->
      printLoop printQueue
    _ -> do
      q `forM_` \t -> print t
      unless (any (\(_, x) -> x == 0) q) $ printLoop printQueue

main :: IO ()
main = do
  counter <- newTVarIO 100
  printQueue <- newTVarIO []
  [1..10] `forM_` \num -> forkIO $ threadLoop num counter printQueue
  printLoop printQueue
