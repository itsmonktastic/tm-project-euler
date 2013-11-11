import Data.List (foldl')

digits n = digits' n []
  where
    digits' 0 acc = acc
    digits' n acc = digits' d (r:acc)
      where
        (d, r) = n `divMod` 10

isPalindrome n = reverse ds == ds
  where
    ds = digits n

maxPalindrome x y
    | isPalindrome y = max x y
    | otherwise      = x

main = print $ foldl' maxPalindrome 0 [x*y | x <- range, y <- range]
  where
    range = [999,998..1]

{-
failed attempt to iterate in order of largest product

3 3
2 2
1 1

3 3
3 2
2 2
3 1
2 1
1 1

maxPalindromeProduct m x y
    | isPalindrome p = p
    | x > y          = traceShow (x, y, p) $ maxPalindromeProduct m (x-1) y
    | x == y         = traceShow (x, y, p) $ maxPalindromeProduct m m (y-1)
  where
    p = x * y
-}