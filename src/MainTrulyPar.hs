import Bruteforce.Common( hash )
import Control.Monad( replicateM )
import Control.Parallel.Strategies( using, parList, rdeepseq )

type Hash = String
type Password = String
type CharList = String

-- | Sequential version of hash bruteforcing.
-- Given a list of candidate passwords, compute
-- hashes and compare to known ones. Return passwords (together with
-- its hashes) that can be found in the list of known hashes.
filterMatched :: [Hash]
              -> [Password]
              -> [(Password, Hash)]
filterMatched knownHashes candidates =
    filter (elemOf knownHashes . snd) pwHashList
  where hashes = map hash candidates
        pwHashList = zip candidates hashes
        elemOf = flip elem

-- | Parallel version of hash bruteforcing.
-- Split password space into chunks and apply
-- sequential bruteforce algorithm to each chunk
-- in parallel.
filterMatchedPar :: Int
                 -> CharList
                 -> [Hash]
                 -> [(Password, Hash)]
filterMatchedPar pwLen charList knownHashes = concat matched
  where prefLen = 1
        grouped = groupedPasswords pwLen prefLen charList
        matched = map (filterMatched knownHashes) grouped
                    `using` parList rdeepseq

-- | Generate passwords of given length, grouped by
-- common prefix (length of a prefix is provided as well).
-- This function is used for parallelization. Each spark
-- should process its own subset of password space, which
-- is generated lazily.
groupedPasswords :: Int -> Int -> CharList -> [[Password]]
groupedPasswords totalLen prefLen charList =
    map (prependPrefix postfixes) prefixes
  where prefixes = replicateM prefLen charList
        postfixes = replicateM restLen charList
        restLen = totalLen - prefLen
        prependPrefix post pre = map (pre ++) post

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

        -- SEQUENTIAL VERSION USAGE EXAMPLE
        -- allPasswords = replicateM pwLen charList
        -- matched = filterMatched hashList allPasswords

        matched = filterMatchedPar pwLen charList hashList

    mapM_ (putStrLn . showMatch) matched

  where showMatch (pw, h) = pw ++ ":" ++ h
