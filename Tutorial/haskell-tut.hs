-- TUTORIAL NOTES
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
2. Open Terminal
3. cd Tutorial
4. Type ghci
5. Type :l haskell-tut

Basic Commands:
1. :t variableName/functionName - Displays the information (type declaration) about the variable/function
2. :l fileName - Load the file
3. :r - Reload the file
4. :q - Quit the Haskell interpreter
2. ctrl + l - Clears the terminal

References:
https://learnyouahaskell.com/chapters
https://youtu.be/02_H3LjqMr8?si=ziU63Rf7kov1N3Ce
-}

-- MODULES
import Data.List -- Provides a lot of functions for lists
import System.IO -- Provides a lot of functions for input/output

-- DATA TYPES
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

-- ARITHMETIC OPERATORS
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

-- BUILT-IN MATH FUNCTIONS
piVal = pi -- 3.141592653589793
ePow9 = exp 9 -- 8103.083927575384
logOf9 = log 9 -- 2.1972245773362196
squared9 = 9 ** 2 -- 81.0
truncateVal = truncate 9.999 -- 9
roundVal = round 9.999 -- 10
ceilingVal = ceiling 9.999 -- 10
floorVal = floor 9.999 -- 9

-- Also sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh

-- LOGICAL OPERATORS
trueAndFalse = True && False -- False, AND
trueOrFalse = True || False -- True, OR
notTrue = not(True) -- False, NOT

-- LISTS - Linked list
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
evensUpTo20 = takeWhile (<=20) [2,4..] -- [2,4,6,8,10,12,14,16,18,20], take the values from the list while the condition is true, in this case, the values are less than or equal to 20
multOfList = foldl (*) 1 [2,3,4,5] 
multOfList2 = foldr (*) 1 [2,3,4,5]
-- 120, fold takes a function, a starting value and a list. It applies the function to the starting value and the first element of the list, then feeds the function with the result and the next element of the list, and so on. foldl starts from the left side of the list and foldr starts from the right side of the list

-- LIST COMPREHENSIONS
pow3List = [3^n | n <- [1..10]] -- [3,9,27,81,243,729,2187,6561,19683,59049], list comprehension that takes every value from 1 to 10 and assigns it to n then raises 3 to the power of n
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
{-
[[1,2,3,4,5,6,7,8,9,10],
 [2,4,6,8,10,12,14,16,18,20],
 [3,6,9,12,15,18,21,24,27,30],
 [4,8,12,16,20,24,28,32,36,40],
 [5,10,15,20,25,30,35,40,45,50],
 [6,12,18,24,30,36,42,48,54,60],
 [7,14,21,28,35,42,49,56,63,70],
 [8,16,24,32,40,48,56,64,72,80],
 [9,18,27,36,45,54,63,72,81,90],
 [10,20,30,40,50,60,70,80,90,100]],
 nested list comprehension that creates a multiplication table from 1 to 10
 The outer list comprehension iterates over each element x in the range [1..10].
 The inner list comprehension iterates over each element y in the range [1..10] for each x.
 x * y multiplies each pair of elements x and y.
-}

-- TUPLES - A pair of values, can be different types of data
randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith -- "Bob Smith", get the first value of the tuple
bobsAge = snd bobSmith -- 52, get the second value of the tuple
names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]
namesNAddress = zip names addresses -- [("Bob","123 Main"),("Mary","234 North"),("Tom","567 South")], zip combines the elements of two lists into a list of tuples

-- FUNCTIONS
addMe :: Int -> Int -> Int -- Function declaration, takes two Ints and returns an Int
-- funcName param1 param2 = operations (returned value)
addMe x y = x + y -- Function definition, add x and y
-- e.g. addMe 1 2 -> 3

sumMe x y = x + y -- Inferred type declaration, Haskell can determine the type of the variable stored in it
-- e.g. sumMe 1 2 -> 3

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int) -- Function declaration, takes two tuples and returns a tuple
addTuples (x, y) (x2, y2) = (x + x2, y + y2) -- Function definition, add the values of the tuples
-- e.g. addTuples (1,2) (3,4) -> (4,6)

whatAge :: Int -> String -- Function declaration, takes an Int and returns a String
-- Kind of switch statement in other languages but in Haskell, it is called pattern matching
whatAge 16 = "You can drive" 
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge x = "Nothing important" -- Default case
-- e.g. whatAge 16 -> "You can drive"

factorial :: Int -> Int -- Function declaration, takes an Int and returns an Int
factorial 0 = 1
factorial n = n * factorial (n - 1) -- Recursion, calls the function within itself
-- e.g. factorial 5 -> 120

prodFact n = product [1..n] -- Function definition, product of all the values from 1 to n
-- e.g. prodFact 5 -> 120

isOdd :: Int -> Bool -- Function declaration, takes an Int and returns a Bool
isOdd n -- Function definition using guards (|)
    | n `mod` 2 == 0 = False -- Check if n is even, if it is return False. Guard part of the function
    | otherwise = True -- n is odd, return True. otherwise is a catch all case or default case
-- e.g. isOdd 3 -> True

isEven n = n `mod` 2 == 0 -- Function definition, check if n is even
-- e.g. isEven 4 -> True

whatGrade :: Int -> String -- Function declaration, takes an Int and returns a String
whatGrade age -- if/else alternative, called guards (|). Reminds me of a ternary operator
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college" -- Default case
-- e.g. whatGrade 5 -> "Kindergarten"

batAvgRating :: Double -> Double -> String -- Function declaration, takes two Doubles and returns a String
batAvgRating hits atBats -- Function with a local variable (avg) using where
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "You're doing pretty good"
    | otherwise = "You're a Superstar"
    where avg = hits / atBats -- where is used to define variables that can be used throughout the function
-- e.g. batAvgRating 100 300 -> "You're doing pretty good"

getListItems :: [Int] -> String -- Function declaration, takes a list of Ints and returns a String
-- Pattern matching
getListItems [] = "Your list is empty" -- If the list is empty
getListItems (x:[]) = "Your list starts with " ++ show x -- If the list has only one value
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y -- If the list has two values
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs -- If the list has more than two values
-- e.g. getListItems [1,2,3] -> "The 1st item is 1 and the rest are [2,3]"

getFirstItem :: String -> String -- Function declaration, takes a String and returns a String
-- Pattern matching
getFirstItem [] = "Empty String" -- If the string is empty
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x] -- If the string is not empty, get the first letter of the string and assign the rest in xs, The complete string is stored in all using aliasing/as (@)
-- e.g. getFirstItem "Hello" -> "The first letter in Hello is H"

-- HIGHER ORDER FUNCTIONS
times4 :: Int -> Int -- Function declaration, takes an Int and returns an Int
times4 x = x * 4 -- Function definition, multiply x by 4
-- e.g. times4 5 -> 20

listTimes4 = map times4 [1, 2, 3, 4, 5] -- [4,8,12,16,20], map applies the function to every element in the list

multBy4 :: [Int] -> [Int] -- Function declaration, takes a list of Ints and returns a list of Ints
multBy4 [] = [] -- If the list is empty, return an empty list, base case
multBy4 (x:xs) = times4 x : multBy4 xs -- Multiply the first value of the list by 4 and add it to the list, then call the function again with the rest of the list

areStringsEq :: [Char] -> [Char] -> Bool -- Function declaration, takes two lists of Chars and returns a Bool
areStringsEq [] [] = True -- If both lists are empty, return True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys -- Check if the first elements of both lists are equal and call the function again with the rest of the lists
areStringsEq _ _ = False -- Catch all case e.g. mismatch length, if the lists are not equal, return False. _ is a wildcard character that matches anything and is used when you don't care about the value
-- e.g. areStringsEq "hello" "hello" -> True

doMult :: (Int -> Int) -> Int -- Function declaration, takes a function that takes an Int and returns an Int and returns an Int
doMult func = func 3 -- Call the function with 3

num3Times4 = doMult times4 -- 12, call the function doMult with the function times4

getAddFunc :: Int -> (Int -> Int) -- Function declaration, takes an Int and returns a function that takes an Int and returns an Int
getAddFunc x y = x + y -- Function definition, add x and y

adds3 = getAddFunc 3 -- Function that adds 3 to a number
fourPlus3 = adds3 4 -- 7, call the function adds3 with 4
threePlusList = map adds3 [1,2,3,4,5] -- [4,5,6,7,8], map applies the function adds3 to every element in the list

-- LAMBDA FUNCTIONS - Anonymous functions (\)
dbl1To10 = map (\x -> x * 2) [1..10] -- [2,4,6,8,10,12,14,16,18,20], map applies the lambda function to every element in the list

-- CONDITIONALS
{-
    < less than,
    > greater than,
    <= less than or equal to,
    >= greater than or equal to,
    == equal to,
    /= not equal to
-}

-- IF STATEMENTS
doubleEvenNumber y =
    if (y `mod` 2 /= 0) -- Check if y is not even
        then y -- If y is not even, return y
        else y * 2 -- If y is even, return y multiplied by 2

getClass :: Int -> String -- Function declaration, takes an Int and returns a String
getClass n = case n of -- Switch statement in Haskell
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away" -- Default case

-- MODULES
{-
    A module is a collection of related functions, types, and typeclasses. A Haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something.

    DEFINING MODULES (Define a module called SampFunctions and export the functions getClass and doubleEvenNumber. Put this at the top of the file): 
    module SampFunctions (getClass, doubleEvenNumber) where 

    IMPORTING MODULES (Import the entire module):
    import SampFunctions
-}

-- ENUMERATED TYPES (ENUMS)
data BaseballPlayer = Pitcher -- Data type declaration, BaseballPlayer is a type that can be Pitcher, Catcher, Infielder, or Outfield
                    | Catcher
                    | Infielder
                    | Outfield
                deriving Show 
                {- 
                    deriving Show is used to display the data type in the console. 
                    Without it, manual implementation of the Show typeclass is required 
                    e.g. instance Show BaseballPlayer where show Outfield = "Outfield"
                -}

barryBonds :: BaseballPlayer -> Bool -- Function declaration, takes a BaseballPlayer and returns a Bool
barryBonds Outfield = True -- If the player is an outfielder, return True

barryInOf = print(barryBonds Outfield) -- True, call the function barryBonds with Outfield

-- CUSTOM DATA TYPES (STRUCTS)
data Customer = Customer String String Double -- Data type declaration, Customer is a type that has a String, String, and Double
    deriving Show -- Show is used to display the data type in the console

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50 -- Create a Customer with the values "Tom Smith" NAME, "123 Main" ADDRESS, and 20.50 BALANCE

getBalance :: Customer -> Double -- Function declaration, takes a Customer and returns a Double
getBalance (Customer _ _ b) = b -- Get the balance of the Customer

data RPS = Rock | Paper | Scissors -- Data type declaration, RPS is a type that can be Rock, Paper, or Scissors

shoot :: RPS -> RPS -> String -- Function declaration, takes two RPS and returns a String
shoot Paper Rock = "Paper beats Rock" -- If the first player chooses Paper and the second player chooses Rock, Paper wins
shoot Rock Scissors = "Rock beats Scissors" -- If the first player chooses Rock and the second player chooses Scissors, Rock wins
shoot Scissors Paper = "Scissors beats Paper" -- If the first player chooses Scissors and the second player chooses Paper, Scissors wins
shoot Scissors Rock = "Rock beats Scissors" -- If the first player chooses Scissors and the second player chooses Rock, Rock wins
shoot Paper Scissors = "Scissors beats Paper" -- If the first player chooses Paper and the second player chooses Scissors, Scissors wins
shoot Rock Paper = "Paper beats Rock" -- If the first player chooses Rock and the second player chooses Paper, Paper wins
shoot _ _ = "Error" -- Catch all case, if the players choose the same thing, return Error
-- e.g. shoot Rock Paper -> "Paper beats Rock"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float -- Data type declaration, Shape is a type that can be a Circle or Rectangle
    deriving Show -- Show is used to display the data type in the console

area :: Shape -> Float -- Function declaration, takes a Shape and returns a Float
area (Circle _ _ r) = pi * r ^ 2 -- Calculate the area of a Circle
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y)) -- Calculate the area of a Rectangle.
--  $ sign can be used to avoid parentheses e.g. area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y), $ means anything that comes after it takes precedence over anything that comes before it

sumValue = putStrLn (show (1 + 2)) -- 3, show is used to convert the result of the operation to a string
sumValue2 = putStrLn . show $ 1 + 2 -- 3, . is used to chain functions together, $ is used to avoid parentheses

areaOfCircle = area (Circle 50 60 20) -- 1256.637, calculate the area of a Circle
areaOfRect = area (Rectangle 10 10 100 100) -- 8100.0, calculate the area of a Rectangle

data Employee = Employee {
    name :: String,
    position :: String,
    idNum :: Int
} deriving (Eq, Show) -- Eq is used to compare two values for equality, Show is used to display the data type in the console
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000} -- Create an Employee with the values "Sam Smith" NAME, "Manager" POSITION, and 1000 ID
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001} -- Create an Employee with the values "Pam Marx" NAME, "Sales" POSITION, and 1001 ID

isSamPam = samSmith == pamMarx -- False, compare the two Employees

samSmithData = show samSmith -- "Employee {name = \"Sam Smith\", position = \"Manager\", idNum = 1000}", display the Employee in the console

data ShirtSize = S | M | L -- Data type declaration, ShirtSize is a type that can be S, M, or L

-- CUSTOM EQUALITY TYPECLASS
instance Eq ShirtSize where -- Overriding the equality function for ShirtSize
    S == S = True -- If the ShirtSize is S, return True
    M == M = True -- If the ShirtSize is M, return True
    L == L = True -- If the ShirtSize is L, return True
    _ == _ = False -- Catch all case, if the ShirtSize is not S, M, or L, return False

-- CUSTOM SHOW TYPECLASS
instance Show ShirtSize where -- Overriding the show function for ShirtSize
    show S = "Small" -- If the ShirtSize is S, return "Small"
    show M = "Medium" -- If the ShirtSize is M, return "Medium"
    show L = "Large" -- If the ShirtSize is L, return "Large"

smallAvail = S `elem` [S, M, L] -- True, check if S is in the list
theSize = show S -- "Small", display the ShirtSize in the console

-- CUSTOM TYPE CLASSES (INTERFACES)
class MyEq a where -- Type class declaration, MyEq is a type class that takes a type a
    areEqual :: a -> a -> Bool -- Function declaration, takes two values of type a and returns a Bool. Must be implemented by the instances of the type class

instance MyEq ShirtSize where
    areEqual S S = True -- If the ShirtSize is S, return True
    areEqual M M = True -- If the ShirtSize is M, return True
    areEqual L L = True -- If the ShirtSize is L, return True
    areEqual _ _ = False -- Catch all case, if the ShirtSize is not S, M, or L, return False

newSize = areEqual M M -- True, check if M is equal to M

-- FILE I/O
-- import System.IO
sayHello = do -- do is used to define a block of code/actions
    putStrLn "What's your name?" -- putStrLn is used to print a string to the console
    name <- getLine -- getLine is used to get input from the user
    putStrLn $ "Hello " ++ name -- Concatenate the string with the input from the user
    -- e.g. sayHello -> What's your name? -> John -> Hello John

writeToFile = do
    theFile <- openFile "test.txt" WriteMode -- Open the file in WriteMode
    hPutStrLn theFile ("Random line of text") -- Write to the file
    hClose theFile -- Close the file
    -- e.g. writeToFile -> Creates a file called test.txt with the text "Random line of text"

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode -- Open the file in ReadMode
    contents <- hGetContents theFile2 -- Get the contents of the file
    putStr contents -- Print the contents of the file
    hClose theFile2 -- Close the file
    -- e.g. readFromFile -> Reads the contents of the file test.txt

-- FIBONACCI SEQUENCE
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)] -- Infinite list of Fibonacci numbers
{-
    zip combines the elements of two lists into a list of tuples
    tail returns all the elements of the list except the first one
    e.g. take 10 fib -> [1,1,2,3,5,8,13,21,34,55]
    SAMPLE DRY RUN:
    1. fib = 1 and (tail fib) = 1
        [1, 1, 2] : a: 1 + b: 1 = 2

    2. fib = 1 and (tail fib) = 2
        [1, 1, 2, 3] : a: 1 + b: 2 = 3
-}
fib300 = fib !! 300 -- 222232244629420445529739893461909967206666939096499764990979600, get the value at index 300

-- MAIN FUNCTION
{-
    main - Entry point of the program in Haskell. When the program is compiled and run, the main function is executed.
    do - Chain a whole bunch of different commands and store them inside of main
    Compile and Run Guide:
    1. Open Terminal
    2. Type ghc --make haskell-tut
    3. Type ./haskell-tut
-}
main = do
    putStrLn "Hello, World" -- Print "Hello, World" to the console