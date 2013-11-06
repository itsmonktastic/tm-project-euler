import qualified Data.HashMap.Lazy as HM
import System.Environment
import Data.List

type Cache = HM.HashMap Integer Int

cacheLookup :: Integer -> Cache -> Maybe Int
cacheLookup = HM.lookup

cacheInsert :: Integer -> Int -> Cache -> Cache
cacheInsert = HM.insert

cacheEmpty :: Cache
cacheEmpty = HM.empty

collatzLength :: Cache -> Integer -> (Cache, Int)
collatzLength cache startN = cacheResult $ collatzLength' 1 startN
  where
    collatz n
        | n <= 0    = error "collatz sequence only defined for positive numbers"
        | odd n     = 3 * n + 1
        | otherwise = n `div` 2

    collatzLength' acc n
        | n == 1    = acc
        | otherwise = useCacheOrContinue acc n

    useCacheOrContinue acc n = case cacheLookup n cache of
        Nothing             -> collatzLength' (acc+1) (collatz n)
        (Just cachedLength) -> (acc + cachedLength)

    cacheResult result = (cacheInsert startN result cache, result)

type CollatzAccumulator = (Cache, Int, Integer)

maxCollatzFold :: CollatzAccumulator -> Integer -> CollatzAccumulator
maxCollatzFold (mem, maxOutput, maxInput) nextInput
    | nextOutput > maxOutput = (nextMem, nextOutput, nextInput)
    | otherwise              = (nextMem, maxOutput, maxInput)
  where
    (nextMem, nextOutput) = collatzLength mem nextInput

longestCollatzSeed :: Integer -> Integer
longestCollatzSeed upper =
    maxInput
  where
    initInput = 1
    (initCache, initLength) = collatzLength cacheEmpty 1
    range = [(initInput+1)..upper]
    folded = foldl' maxCollatzFold (initCache, initLength, initInput) $ range
    (_, _, maxInput) = folded

main :: IO ()
main = do
    (upper:_) <- getArgs
    print $ longestCollatzSeed $ read upper
