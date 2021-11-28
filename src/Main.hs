import Bruteforce.Common
 
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
      charList = ['0'..'9'] ++ ['a'..'z']
      pwHashList = [(pw,hash pw) | pw <- passwordList charList pwLen]
      rslt = [pw ++ ":" ++ h | (pw,h) <- pwHashList, h `elem` hashList]
  mapM_ putStrLn {- $ take (length hashList) -} rslt
