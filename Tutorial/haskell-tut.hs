{-
Lazy Programming it does not execute more than it needs to
Functional Programming and everything in it are IMMUTABLE
Recursion is used very often in Haskell
NO for/while loops or technically variables, only constants
A great language to learn to get better at programming
Extremely safe/strict language does it's best to cut out any potential errors
Type Inference - Haskell can determine the type of the variable stored in it but it is recommended to declare the type of the variable
Statically Typed - Once a type is assigned to a variable, it cannot be switched

Setup:
1. Install Haskell Platform
2.1 Open Terminal
2.2 cd Tutorial
3. Type ghci
4.1 Type :l haskell-tut
4.2 Type :r to reload the file
5. Type :q to exit

Basic Commands:
1. :t variableName/functionName - Displays the information about the variable/function
2. ctrl + l - Clears the terminal

References:
https://learnyouahaskell.com/chapters
https://youtu.be/02_H3LjqMr8?si=ziU63Rf7kov1N3Ce
-}

-- Modules
import Data.List -- Provides a lot of functions for lists
import System.IO -- Provides a lot of functions for input/output

-- Num - General number type (Int, Integer, Float, Double)
-- Int -2^63 to 2^63
-- Integer (Unbounded whole number)
maxInt = maxBound :: Int -- 9223372036854775807

-- Float (Single precision floating point number)
-- Double (Double precision floating point number)
bigFloat = 3.99999999999 + 0.00000000005 -- 4.00000000004
bigFloat2 = 3.999999999999 + 0.000000000005 -- 4.0000000000039995

-- Bool True False
truthy = True
falsy = False
-- Char '
letterA = 'A'
-- Tuple (Two values)
tuple = (1,2)

always5 :: Int
always5 = 5

sumOfNums = sum [1..1000] -- 500500, Sums up all the numbers from 1 to 1000

-- Operators
addEx = 5 + 4 -- 9
subEx = 5 - 4 -- 1
multEx = 5 * 4 -- 20
divEx = 5 / 4 -- 1.25

modEx = mod 5 4 -- 1, prefix operator
modEx2 = 5 `mod` 4 -- 1, infix operator

negNumEx = 5 + (-4) -- 1, When using negative numbers, use parentheses

num9 = 9 :: Int
-- :t sqrt
-- sqrt :: Floating a => a -> a
sqrtOf9 = sqrt (fromIntegral num9) -- 3.0, fromIntegral converts Int to floating point data type

-- Built in math functions
piVal = pi -- 3.141592653589793
ePow9 = exp 9 -- 8103.083927575384
logOf9 = log 9 -- 2.1972245773362196
squared9 = 9 ** 2 -- 81.0
truncateVal = truncate 9.999 -- 9
roundVal = round 9.999 -- 10
ceilingVal = ceiling 9.999 -- 10
floorVal = floor 9.999 -- 9

-- Also sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh

-- Logical Operators
trueAndFalse = True && False -- False
trueOrFalse = True || False -- True
notTrue = not(True) -- False

-- Lists - Linked list
-- Lists are homogenous, they can only store one type of data
-- A value can only be added at the beginning/front of the list
primeNumbers = [3,5,7,11] -- [3,5,7,11]
favNums = 2 : 7 : 21 : 66 : [] -- [2,7,21,66], : is the cons operator, combine numbers into a list
morePrimes = primeNumbers ++ [13,17,19,23,29] -- [3,5,7,11,13,17,19,23,29], ++ is the concatenation operator
multList = [[3,5,7],[11,13,17]] -- [[3,5,7],[11,13,17]], nested list
morePrimes2 = 2 : morePrimes -- [2,3,5,7,11,13,17,19,23,29], Combine lists, basically append 2 to the front of morePrimes
lenPrime = length morePrimes2 -- 10, length of the list
revPrime = reverse morePrimes2 -- [29,23,19,17,13,11,7,5,3,2], reverse the list
isListEmpty = null morePrimes2 -- False, check if the list is empty
secondPrime = morePrimes2 !! 1 -- 3, get the value at index 1
firstPrime = head morePrimes2 -- 2, get the first value
lastPrime = last morePrimes2 -- 29, get the last value
primeInit = init morePrimes2 -- [2,3,5,7,11,13,17,19,23], get all the values except the last one
first3Primes = take 3 morePrimes2 -- [2,3,5], get the first 3 values
removePrimes = drop 3 morePrimes2 -- [7,11,13,17,19,23,29], remove the first 3 values
is7InList = 7 `elem` morePrimes2 -- True, check if 7 is in the list
maxPrime = maximum morePrimes2 -- 29, get the maximum value
minPrime = minimum morePrimes2 -- 2, get the minimum value
newList = [2,3,5] -- [2,3,5]
prodPrimes = product newList -- 30, get the product of all the values in the list
zeroToTen = [0..10] -- [0,1,2,3,4,5,6,7,8,9,10], create a list from 0 to 10
evenList = [2,4..20] -- [2,4,6,8,10,12,14,16,18,20], create a list of even numbers from 2 to 20, the .. operator is used to create a list with a step value defined by the difference between the first two numbers |2 - 4| = 2
letterList = ['A','C'..'Z'] -- "ACEGIKMOQSUWY", create a list of letters from A to Z with a step value of |'A' - 'C'| = 2
infinPow10 = [10,20..] -- Infinite list but because Haskell is lazy, it will only generate the values that are needed and does not generate the entire list until it is needed
many2s = take 10 (repeat 2) -- [2,2,2,2,2,2,2,2,2,2], repeat 2 and take the first 10 values, basically repeat 2 ten times
many3s = replicate 10 3 -- [3,3,3,3,3,3,3,3,3,3], replicate 3 ten times
cyclelist = take 10 (cycle [1, 2, 3, 4, 5]) -- [1,2,3,4,5,1,2,3,4,5], cycle through the list indefinitely and take the first 10 values
listTimes2 = [x * 2 | x <- [1..10]] -- [2,4,6,8,10,12,14,16,18,20], this is a list comprehension which pull every element in the list from 1 to 10 and assign it to x then multiply each value by 2
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50] -- [3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48], list comprehension with a condition, only multiply the value by 3 if it is less than or equal to 50
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0] -- [117,234,351,468], list comprehension with multiple conditions/filters , only get the values from 1 to 500 that are divisible by 13 and 9
sortedList = sort [9,1,8,3,4,7,6] -- [1,3,4,6,7,8,9], sort the list in ascending order
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10] -- [7,9,11,13,15], zipWith combines the elements of two lists by applying a function to them, in this case, it is addition. If the lists are not the same length, the extra elements are ignored
listBiggerThen5 = filter (>5) morePrimes2 -- [7,11,13,17,19,23,29], filter out the values that are greater than 5