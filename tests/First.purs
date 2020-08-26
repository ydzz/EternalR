module Main where

foreign import log::Int -> Int -> Int

constFunc::Int -> Int -> Int
constFunc a b = log b a

{-
constRecord :: { varA :: Int, varB :: String,varC::{a::Int } }
constRecord = {varA : 1,varB : "a",varC: {a: 2 } }

constInt :: Int
constInt = 12450

constArray :: Array Int
constArray = [1,2,3]

constRecord :: { varA :: Int, varB :: String}
constRecord = {varA : 1,varB : "a" }

constArray :: Array Int
constArray = [1,2,3]



constString :: String
constString = "Stringggggggggg"



constInt :: Int
constInt = 12450

constChar :: Char
constChar = '1'

constNumber::Number
constNumber = 3.14159265358

constString :: String
constString = "Stringggggggggg"-}

{-
constArray :: Array Int
constArray = [1,2,3]



mularg::Int -> Int -> Int
mularg n nn = n

class Default a where
    def::a
  
classFunc::forall a.Default a => a -> a
classFunc v = v
-}