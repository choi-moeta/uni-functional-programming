-- import Test.Tasty
import Test.HUnit

import Data.List
import Data.Ord

import Convert
import Threads

test_convert1 = TestCase $ assertEqual "change case" "CASE" (convert "case")
test_convert2 = TestCase $ assertEqual "in quotes" "'quotes'NOT" (convert "'quotes'not")
test_convert3 = TestCase $ assertEqual "in apostrophes" "`apos`NOT" (convert "`apos`not")
test_convert4 = TestCase $ assertEqual "in double brackets" "(brkt)NOT" (convert "(brkt)not")
test_convert11 = TestCase $ assertEqual "in double brackets" ")brkt(" (convert ")brkt(")
test_convert5 = TestCase $ assertEqual "trim spaces" " TRIM" (convert " trim   ")
test_convert6 = TestCase $ assertEqual "tab to 2 spaces" "A  B" (convert ("a" ++ "\t" ++ "b"))
test_convert7 = TestCase $ assertEqual "unchagable symbols" "12.!@#\\" (convert "12.!@#\\")
test_convert8 = TestCase $ assertEqual "blank string" "" (convert "")
test_convert9 = TestCase $ assertEqual "trim to blank" "" (convert "       ")
test_convert10 = TestCase $ assertEqual "only english" "слово" (convert "слово") 

tests = TestList [
    test_convert1,
    test_convert2,
    test_convert3,
    test_convert4,
    test_convert5,
    test_convert6,
    test_convert7,
    test_convert8,
    test_convert9,
    test_convert10,
    test_convert11]

main = runTestTT tests