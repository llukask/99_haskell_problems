-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()
main = do
    print (myLast [1,2,3,4])
    print (myButLast [1,2,3,4])
    print (elementAt [1,2,3,4] 2)
    print (myLength [1,2,3,4])
    print (myReverse [1,2,3,4])
    print (isPalindrome [1,2,3])
    print (isPalindrome [1,2,4,8,16,8,4,2,1])

--problem 1
myLast :: [a] -> a
myLast = head . reverse

--problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

--problem 3
elementAt :: (Integral b) => [a] -> b -> a
elementAt list 0 = head list
elementAt list 1 = head list
elementAt list n = elementAt (tail list) (n-1)

--problem 4
myLength :: (Integral b) => [a] -> b
myLength list = myLength' list 0

myLength' :: (Integral b) => [a] -> b -> b
myLength' [] l = l
myLength' list l = myLength' (tail list) (l+1)

--problem 5
myReverse :: [a] -> [a]
myReverse list = myReverse' list []

myReverse' :: [a] -> [a] -> [a]
myReverse' [] reversed = reversed
myReverse' list reversed = myReverse' (tail list) ((head list):reversed)

--problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome list
    | firstIsLast       = isPalindrome (tail (init list))
    | not firstIsLast   = False
    where firstIsLast = head list == last list

--problem 8
compress :: (Eq a) => [a] -> [a]
compress list = compress' list (head list)

compress' :: (Eq a) => [a] -> a -> [a]
compress' list a = list


