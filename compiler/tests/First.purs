module Main where

newtype Player = Player {
    name::String,
    age::Int
}

data Maybe a = Just a | Nothing

myInt::Int
myInt = 123

swap::Maybe Int -> Int -> Int
swap Nothing i2 = i2
swap (Just i) i2 = i