module Main (main,Fuck) where

foreign import log_int::Int -> Int

foreign import prim_int_add_::Int -> Int -> Int
infixl 6 prim_int_add_ as +


data Fuck = FuckA Int | FuckB

newtype Player = Player { name::String }

constF :: Fuck
constF = let a = 100 + b in FuckA a
 where
   b = 1

--注释comment aaa
main::Int
main = log_int (2 + 6 + a)
 where
  a = 555 + b
  b = 11

--std_int_prim.wrapping_add 667 114514

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