import System.Environment

fibonacci = 1:2:zipWith (+) fibonacci (tail fibonacci)

takeEvery n [] = []
takeEvery n orig@(x:xs) = x:takeEvery n (drop n orig)

evenFibSum limit = sum $ takeEvery 3 $ tail $ takeWhile (<= limit) $ fibonacci

main = do
  (n:_) <- getArgs
  print $ evenFibSum $ read n