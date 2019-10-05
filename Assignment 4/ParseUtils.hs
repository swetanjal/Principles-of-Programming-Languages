
module ParseUtils where

checkLen :: [a] -> Int -> Bool
checkLen ls n = (length ls) == n

isEq :: String -> String -> Bool
isEq s s_ = s == s_

numbers :: String
numbers = "1234567890"

letters :: String
letters = "abcdefghijklmnopqrstuvwxyz"

isDigit :: Char -> Bool
isDigit d = d `elem` numbers

isAlphaDigit :: Char -> Bool
isAlphaDigit d = (d `elem` numbers) || (d `elem` letters)

isNumber :: String -> Bool
isNumber n = foldr (&&) True $  map isDigit n

isAlNum :: String -> Bool
isAlNum s = foldr (&&) True $  map isAlphaDigit s

isBoolean :: String -> Bool
isBoolean s = (s == "True") || (s == "False")

isBinaryOp :: String -> Bool
isBinaryOp s = s `elem` ["=", "+", "-", "*", "/", "&", "|"]

isUnaryOp :: String -> Bool
isUnaryOp s = s `elem` ["~", "zero?"]

isOtherKeyw :: String -> Bool
isOtherKeyw s = s `elem` ["assume", "if"]

isKey :: String -> Bool
isKey s = (isBinaryOp s) || (isUnaryOp s) || (isOtherKeyw s)
