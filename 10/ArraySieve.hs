import Data.Array.ST
import Control.Monad.ST
import System.Environment

primeArray :: Integer -> ST s (STArray s Integer Bool)
primeArray n = newArray (2, n) True

sieveST :: Integer -> ST s [Integer]
sieveST n = do
    arr <- primeArray n
    sieveLoop arr 2
    filterArr 2 arr []
  where
    filterArr i arr acc
        | i > n     = return acc
        | otherwise = do
            p <- readArray arr i
            filterArr (i+1) arr (if p then (i:acc) else acc)                

    sieveLoop arr i
        | i > n     = return ()
        | otherwise = do
            isPrime <- readArray arr i
            innerLoopIfPrime arr i isPrime

    sieveInnerLoop arr i j
        | i > n     = sieveLoop arr (j + 1)
        | otherwise = do
            writeArray arr i False
            sieveInnerLoop arr (i+j) j

    innerLoopIfPrime arr i p
        | p         = sieveInnerLoop arr (i+i) i
        | otherwise = sieveLoop arr (i+1)

primeSieve n = runST $ sieveST n

main = do
    (n:_) <- getArgs
    print $ sum $ primeSieve $ read n