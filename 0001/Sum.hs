import System.Environment (getArgs)

-- bit of maths to make sum calculation more efficient
--
-- sum [1..n] = n(n+1)/2
-- sum [3,6..n] = 3(sum [1,2..(n/3)]) = 3((n/3)(n/3+1)/2)
-- sum [m*1,m*2,..(m*n/m)] = m(sum [1,2,..(n/m)]) = (m)(n/m)(n/m+1)/2 = n(n/m+1)/2
multipleSum m n = m * (l `div` m) * ((l `div` m) + 1) `div` 2
  where
    l = n - 1

main = do
  (nStr:_) <- getArgs
  let n = read nStr
  print $ multipleSum 3 n + multipleSum 5 n - multipleSum 15 n