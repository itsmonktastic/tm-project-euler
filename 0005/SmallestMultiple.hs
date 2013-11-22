n = 20
candidates = [n, n*2..]
answer = head $ filter divisibleByAll candidates
divisors = [2..(n-1)]
divisibleByAll x = all (\d -> x `mod` d == 0) divisors

main :: IO ()
main = do
    print answer
