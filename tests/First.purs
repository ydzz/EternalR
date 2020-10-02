module Main(IO,main) where

foreign import primcore'int_add::Int -> Int -> Int
foreign import primcore'int_eq::Int -> Int -> Boolean
foreign import primcore'int_less::Int -> Int -> Boolean
infixl 6 primcore'int_add as +
infixl 6 primcore'int_eq as ==
infixl 6  primcore'int_less as <
infixl 6 discard  as >>=
foreign import data IO::Type -> Type

foreign import io'bind::forall a b.  (a -> IO b) -> IO a -> IO b
foreign import io'pure::forall a .a -> IO a
foreign import io'println::String -> IO Int
foreign import io'randomInt::IO Int
foreign import io'showInt::Int -> String

class Monad m where
    discard ::forall a b. m a -> (a -> m b) -> m b
    pure::forall a.a -> m a

class Show a where
    show::a -> String

instance showInt :: Show Int where
    show = io'showInt

instance ioMonad :: Monad IO where
    discard m f = io'bind f m
    pure = io'pure

bind :: forall a b m. Monad m => m a -> (a -> m b) -> m b
bind = discard

putStrLn::String -> IO Int
putStrLn = io'println

randomInt :: IO Int
randomInt = io'randomInt
{-----------------------------------------------------------------------}
testIf2::Int
testIf2 = if 2 < 100 then 1 else 2

testIf::Int -> IO Int
testIf n = do
            putStrLn "testIf"
            if n < 100 
            then do
              putStrLn "小于100"
              putStrLn (show n)
            else do 
              putStrLn "大于或等于100"
              putStrLn (show n)

testLet::Int -> Int
testLet n = let constIdx = 666
                const2   = 334
                hhhh     = 111
            in constIdx + const2 + n + hhhh

testWhereLet::Int -> Int
testWhereLet n = numa + numb + n
  where
    numa = 1 + 2
    numb = 1

testMonadLet::IO Int
testMonadLet = do
  putStrLn "testMonadLet"
  rNumber <- randomInt
  let strNumber = show rNumber
  let str2 = "测试String"
  putStrLn strNumber
  putStrLn str2

testMulitIf::Int -> String
testMulitIf n 
              |  n < 100 = "Zero"
              | true = "Other"

main::IO Int
main = do
  rNumber <- randomInt
  testIf rNumber
  putStrLn "testLet"
  putStrLn (show (testLet 111))
  putStrLn "testWhereLet"
  putStrLn (show (testWhereLet 1))
  testMonadLet
  putStrLn "testMulitIf"
  putStrLn (testMulitIf rNumber)
  pure 0
