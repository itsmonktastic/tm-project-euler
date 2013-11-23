upper :: Integer
upper = 100

range = [1..upper]
squareSum = (^2) $ sum range
sumOfSquares = sum $ map (^2) $ range
answer = squareSum - sumOfSquares

main = do
    print answer
