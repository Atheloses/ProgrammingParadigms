import Data.Char

--Create a function that takes first n elements of the list
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x: take' (n-1) xs


--Create a function that takes the remaining list after the first n elements
drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs


--Create a function that find the smallest element in the list. Consider input restrictions
--minimum' :: [a] -> a


--Create a function that merge two lists into one list of tuples
zipThem :: [a] -> [b] -> [(a,b)]
zipThem (y:yz) (x:xs) = (y,x): zipThem yz xs
zipThem _ _ = []


--Create a function that compute Cartesian product of two vectors
--dotProduct :: [a] -> [b] -> [(a,b)]


--Create a function that computes n-th number in the Fibonacci sequence. The function should use tuples in the solution. 
--fibonacci :: Int -> Int


--Create a function that takes a string and converts all characters to upper case letters
allToUpper :: String -> String
allToUpper xs = [toUpper x |x<-xs] 
