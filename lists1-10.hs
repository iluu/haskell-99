module Lists where

-- Problem 1: Find the last element of a list
myLast :: [a] -> a
myLast xs = case xs of [] -> error "Empty list"
                       ([x]) -> x
                       (_:ys) -> myLast ys

-- Problem 2: Find the last but one element of a list
myButLast :: [a] -> a
myButLast xs = case xs of [] -> error "Empty list"
                          ([_]) -> error "Only one element"
                          (x:_:[]) -> x
                          (_:y:ys) -> myButLast (y:ys)

myButLast' :: [a] -> a
myButLast' xs = last (init xs)

-- Problem 3: Find the nth element of a list. First element on the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt ([]) _ = error "Index out of bounds"
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4: Find the number of elements of a list
myLength :: [a] -> Int
myLength xs = case xs of [] -> 0
                         (_:ys) -> 1 + myLength ys

-- Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse xs = case xs of [] -> []
                          (y:ys) -> myReverse ys ++ [y]
