
--Create a function that computes length of a list.
--length' :: [a] -> Int


-- Create a function that sums the list of integers.
sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs

-- Create a function that returns the first element in the list.
getHead :: [a] -> a
getHead (x:xs) = x

-- Create a function that returns the last element in the list.
getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast(xs)

-- Create a function that checks if an element is a member of the list.
isElement :: Eq a => a -> [a] -> Bool
isElement n [] = False
isElement n (x:xs) | n == x = True
                   |otherwise = isElement n xs 

-- Create a function that returns the list without the first element.
--getTail :: [a] -> [a]


-- Create a function that returns the list without the last element.
--getInit :: [a] -> [a]


--Create a function that merge two lists into one list.
combine :: [a] -> [a] -> [a]
combine [] b = b
combine (x:xs) b = x:combine xs b

-- Create a function that finds the maximum in the list of integers.
--max' :: [Int] -> Int


-- Create a function that reverse a list.
--reverse' :: [a] -> [a]


-- Create a function that product scalar multiplication if two vectors.
--scalar :: [Int] -> [Int] -> Int

