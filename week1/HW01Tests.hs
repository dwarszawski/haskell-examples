-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 4), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits" testToRevDigits
             [(123, [3,2,1]), (1, [1]), (54321, [1,2,3,4,5])]
           ]
-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther" testDoubleEveryOther
           [([1], [1]), ([1,2,3,4], [1,4,3,8])]
           ]
-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = []

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = []

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
