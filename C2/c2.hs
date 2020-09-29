sum' :: Int -> Int -> Int
sum' x y = x + y

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-2)+fib(n-1)

fib' n = fib2 n 1 1 where
    fib2 0 a b = a
    fib2 n a b = fib2 (n-1) b (a + b)

sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs

leapYear :: Int -> Bool
leapYear n = n `mod` 4 == 0 && n `mod` 100 /= 0 || n `mod` 400 == 0

leapYear' :: Int -> Bool
leapYear' n | n `mod` 400 == 0 = True
            | n `mod` 100 == 0 = False
            | otherwise = n `mod` 4 == 0

gcd' :: Int -> Int -> Int
gcd' a b | a == b = a
        | a > b = gcd' (a-b) b
        | a < b = gcd' a (b-a)


getHead :: [a] -> a
getHead (x:xs) = x

getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast(xs)

isElement :: Eq a => a -> [a] -> Bool
isElement n [] = False
isElement n (x:xs) | n == x = True
                   |otherwise = isElement n xs 


combine :: [a] -> [a] -> [a]
combine [] b = b
combine (x:xs) b = x:combine xs b