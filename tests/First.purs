module Main(main) where

--foreign import log_int::Int -> Int
foreign import __prim_int_add::Int -> Int -> Int
infixl 6 __prim_int_add as +

data Maybe a = Just a | Nothing

class Functor f (fc::(Type -> Type) -> Type) where
    fmap::forall a b c.(a -> b) -> f a -> fc c -> f b 
{-
instance functorMaybe :: Functor Maybe where
    fmap f ma = case ma of
                  Just val -> Just (f val)
                  Nothing -> Nothing
-}
main::Int
main = 111
{-
class Show a  where
    show::a -> String
  

instance showInt :: Show Int where
    show a = "Hooooooooooooooooooooooooo!"
-}




--std_int_prim.wrapping_add 667 114514

{-
instance showInt :: Show Int Int Int where
    show i a c = "123"

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