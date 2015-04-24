module Util where

import Debug.Trace
import Data.Char
import Data.List
import Control.Applicative
import Control.Arrow

capitalize :: String -> String
capitalize [] = []
capitalize (a:as) = toUpper a : as
    
mergeColumns :: [[String]] -> [[String]] -> [[String]]
mergeColumns [] b = b
mergeColumns b [] = b
mergeColumns as@(a:_) bs@(b:_) = go <$> zip as' bs'
  where
    i = length a
    as' | length as < length bs = as ++ replicate (length bs - length as) []
        | otherwise = as
            
    bs' | length bs < length as = bs ++ replicate (length as - length bs) []
        | otherwise = bs  
            
    go (as, bs)
      | length as < i = as ++ replicate (i - length as) "" ++ bs
      | length as > i = take i as ++ bs
      | otherwise = as ++ bs

mergeManyColumns :: [[[String]]] -> [[String]]
mergeManyColumns = foldl mergeColumns []

replaceElem :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem fd fu (x:xs) 
  | fd x = fu x : replaceElem fd fu xs
  | otherwise = x : replaceElem fd fu xs

trace' :: Show a => a -> a
trace' a = traceShow a a

replace :: Int -> a -> [a] -> [a]
replace i a as = take i as ++ [a] ++ drop (i+1) as

removeItem :: Int -> [a] -> [a]
removeItem i as = take i as ++ drop (i+1) as

avg :: Integral a => [a] -> a
avg as = sum as `ceilDiv` fromIntegral (length as)

ceilDiv :: Integral a => a -> a -> a
ceilDiv a b = ceiling (fromIntegral a / fromIntegral b)

fetchElements :: Eq a => [a] -> [a] -> ([a] , [a])
fetchElements what from = foldl go ([], from) what
  where go (acc, left) a = first (maybe acc (:acc)) $ fetchOne a left
            
fetchOne :: Eq a => a -> [a] -> (Maybe a, [a])
fetchOne a as 
  | a `elem` as = case find (==a) as of
    Nothing -> (Nothing, as)
    Just a' -> (Just a', filter (/=a) as)
  | otherwise = (Nothing, as)

fetchOne' :: (a -> Bool) -> [a] -> (Maybe a, [a])
fetchOne' f [] = (Nothing, [])
fetchOne' f (a:as) 
  | f a = (Just a, as)
  | otherwise = let (res, as') = fetchOne' f as in (res, a:as')