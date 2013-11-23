-- 231472987321283 causes segfault
-- 23147298732128333 causes negative size range

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

primeSieve :: Integer -> [Integer]
primeSieve n = runST $ sieveST n

main = do
    (nStr:_) <- getArgs
    let n = read nStr
    print $ guessLoop n (n*2)

guessLoop :: Integer -> Integer -> Integer
guessLoop n g
    | numOfPrimes > n = head $ drop (fromInteger $ numOfPrimes - n) $ primes
    | otherwise       = guessLoop n (g + n)
  where
    primes      = primeSieve g
    numOfPrimes = toInteger $ length primes

