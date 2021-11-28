import Bruteforce.Common
import Control.Concurrent
import Control.Monad
import Control.DeepSeq

workerLoop :: MVar [String] -> MVar [ [String] ] -> String -> Int ->
                [String] -> IO ()
workerLoop taskQueue resultQueue charList pwLen hashList = do
  maybeTask <- modifyMVar taskQueue
                 (\q -> return $ case q of
                                   [] -> ([], Nothing)
                                   (x:xs) -> (xs, Just x))
  case maybeTask of
    Nothing -> return ()
    Just task -> do
      let postfixList = passwordList charList $ pwLen - length task
          pwList = map (task ++) postfixList
          pwHashList = [(pw, hash pw) | pw <- pwList]
          rslt = [pw ++ ":" ++ h | (pw,h) <- pwHashList,
                                    h `elem` hashList] 
      rslt `deepseq` modifyMVar_ resultQueue (\q -> return $ rslt:q) 
      workerLoop taskQueue resultQueue charList pwLen hashList

mainLoop :: MVar [ [String] ] -> Int -> IO ()
mainLoop _ 0 = return ()
mainLoop resultQueue taskNumber = do
  results <- modifyMVar resultQueue (\q -> return ([], q))
  case results of
    [] -> do
      threadDelay 100000 -- 100 ms
      mainLoop resultQueue taskNumber
    _ -> do
      mapM_ (mapM_ putStrLn) results
      mainLoop resultQueue (taskNumber - length results)

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
  taskQueue <- newMVar taskList
  resultQueue <- newMVar []
  workerNumber `replicateM_` forkIO (workerLoop taskQueue resultQueue
                                             charList pwLen hashList)
  mainLoop resultQueue taskNumber
