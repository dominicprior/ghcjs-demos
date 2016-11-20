{-
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where
import Lucid.Base
import Lucid.Html5
--import Control.Monoid
-}

class L t where len :: t -> Int
instance L Char where len _ = 1
instance L Bool where len _ = 33
instance L [a] where len xs = length xs
--instance L (a -> b) where len _ = 5
instance L (a,b) where len _ = 2
instance L (Maybe a) where len _ = 3
instance (L r) => L (a -> r) where len f = 8

class C t where
  f' :: [String] -> t

instance C Char where
  f' strings = head $ head strings

instance C Int where
  f' strings = length strings

instance (Show a, C r) => C (a -> r) where
  f' strings = \x -> f' (strings ++ [show x])

g :: [String] -> Int
g = f'

--h :: (Show a, C r) => [String] -> (a -> r)
--h strings = f' strings

instance C (IO a) where
  f' strings = do
    putStrLn "-----"
    --putStrLn $ strings !! 0
    print strings
    --mapM_ putStrLn strings
    --putStrLn $ "len = " ++ show (length strings)
    putStrLn "-----"
    return undefined
{-
f :: (C t) => t
f = f' []
-}

main :: IO ()
main = f' [] True 99 "Mary" 'k' "had"