import Bruteforce.Common
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

workerLoop :: TQueue String -> TQueue [String] -> String -> Int ->
                [String] -> IO ()
workerLoop taskQueue resultQueue charList pwLen hashList = do
  maybeTask <- atomically $ tryReadTQueue taskQueue
  case maybeTask of
    Nothing -> return ()
    Just task -> do
      let postfixList = passwordList charList $ pwLen - length task
          pwList = map (task ++) postfixList
          pwHashList = [(pw, hash pw) | pw <- pwList]
          rslt = [pw ++ ":" ++ h | (pw,h) <- pwHashList,
                                    h `elem` hashList] 
      rslt `deepseq` atomically $ writeTQueue resultQueue rslt
      workerLoop taskQueue resultQueue charList pwLen hashList             

mainLoop :: TQueue [String] -> Int -> IO ()
mainLoop _ 0 = return ()
mainLoop resultQueue taskNumber = do
  res <- atomically $ readTQueue resultQueue
  mapM_ putStrLn res
  mainLoop resultQueue (taskNumber - 1)

main :: IO ()
main = do
  let hashList = [
        -- 1234
        "03ac674216f3e15c761ee1a5e255f067" ++
        "953623c8b388b4459e13f978d7c846f4",
        -- r2d2
        "8adce0a3431e8b11ef69e7f7765021d3" ++
        "ee0b70fff58e0480cadb4c468d78105f"
        ]
      pwLen = 4
      chunkLen = 2
      charList = ['0'..'9'] ++ ['a'..'z']
      taskList = passwordList charList chunkLen
      taskNumber = length taskList
  workerNumber <- getNumCapabilities
  taskQueue <- newTQueueIO
  resultQueue <- newTQueueIO
  atomically $ taskList `forM_` writeTQueue taskQueue
  workerNumber `replicateM_` forkIO (workerLoop taskQueue resultQueue
                                             charList pwLen hashList)
  mainLoop resultQueue taskNumber
