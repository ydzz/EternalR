module Main(pi) where

--foreign import log_int::Int -> Int
foreign import __prim_int_add::Int -> Int -> Int
infixl 6 __prim_int_add as +
infixl 6 discard  as >>=
foreign import data IO::Type -> Type
foreign import io'__io_bind::forall a b. IO a -> (a -> IO b) -> IO b
foreign import io'__io_pure::forall a .a -> IO a
--foreign import io'readLine::IO String
--foreign import io'__println::String -> IO Int

pi::Number
pi = 3.14159265358

class Monad m where
    discard ::forall a b. m a -> (a -> m b) -> m b
    --pure::forall a.a -> m a

instance ioMonad :: Monad IO where
    discard  = io'__io_bind
    --pure = io'__io_pure

--main::IO Int
--main = do
  --io'println "fuck world"
--  pure 0
{-
data Maybe a = Just a | Nothing

class Monad m where
    bind::forall a b. m a -> (a -> m b) -> m b

instance maybeMonad:: Monad Maybe where
    bind maya bindf = case maya of
                        Just v  -> bindf v
                        Nothing -> Nothing

unwrap::Maybe Int -> Int
unwrap Nothing = 0
unwrap (Just idx) = idx

defaultOne::Maybe Int
defaultOne = Just 114514

add1::Int -> Maybe Int
add1 n = Just (n + 1)

toN::Int -> Maybe Int
toN _ = Nothing

doEffect::Maybe Int
doEffect = do
   v <- defaultOne
   v2 <- add1 v
   v3 <- add1 v2
   vn <- toN v3
   add1 v3

main::Int
main = unwrap doEffect


class Functor f where
    fmap::forall a b.(a -> b) -> f a -> f b 

instance functorMaybe :: Functor Maybe where
    fmap f ma = case ma of
                  Just val -> Just (f val)
                  Nothing -> Nothing
addOne::Int -> Int
addOne a = a + 1
-}

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