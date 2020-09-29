-- Function that computes a factorial of a given number. 
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Function that computes n-th number in Fibonacci sequence.
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-2)+fib(n-1)

fib' n = fib2 n 1 1 where
    fib2 0 a b = a
    fib2 n a b = fib2 (n-1) b (a + b)


-- Function that checks if a year is a leap-year (divisible without remainder by 4 and it is not divisible by 100. If it is divisible by 400, it is a leap-year).
leapYear :: Int -> Bool
leapYear n = n `mod` 4 == 0 && n `mod` 100 /= 0 || n `mod` 400 == 0

leapYear' :: Int -> Bool
leapYear' n | n `mod` 400 == 0 = True
            | n `mod` 100 == 0 = False
            | otherwise = n `mod` 4 == 0

-- Implement two functions that returns a maximum from 2 respectively 3 given parameters.
--max2 :: Int -> Int -> Int
--max3 :: Int -> Int -> Int -> Int


-- Term combination is a selection of items from a collection, such that (unlike permutations) the order of elements in 
-- this selection does not matter. Compute the number of possible combinations if we are taking k things from the collection of n things.
--combinations :: Int -> Int -> Int


-- Implement a function that computes the number of solutions for a quadratic equation. This quadratic equation will be given using standard coefficients: a, b, c.
--numberOfRoots :: Int -> Int -> Int -> Int


-- Implement a function that computes greatest common divider for two given numbers.
gcd' :: Int -> Int -> Int
gcd' a b | a == b = a
        | a > b = gcd' (a-b) b
        | a < b = gcd' a (b-a)


-- Implement a function that compute, if a given number is a prime number.
--isPrime :: Int -> Bool
