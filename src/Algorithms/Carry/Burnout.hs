module Algorithms.Carry.Burnout where

import Types
import Products
import Algorithms.ProductList
import Algorithms.Carry.List
import Util

import Data.Csv
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS

-- Returns sequence of points (meal number, gramms left in ration)
burnoutPlot :: [Person] -> Ration -> [(Int, Double)]
burnoutPlot persons ration = fst $ foldl go ([], totalGrammsBegin) $ zip [1..] meals
  where
    totalGrammsBegin = sumGramms plist
    meals = concat $ dayProducts <$> ration
    plist = filter notReplacement $ totalProductList persons ration
    notReplacement p = productType (boxedProduct p) /= Replacement
    sumGramms pl = sum $ boxedTotalGramm <$> plist
    
    go :: ([(Int, Double)], Int) -> (Int, [Product]) -> ([(Int, Double)], Int)
    go (points, totalGramms) (i, meal) = (points ++ [(i, loadPercent)], newTotalGramms)
      where
        mealGramms = sum $ (\p -> productGramm p * personsWhoEat persons p) <$> meal
        newTotalGramms = totalGramms - mealGramms
        loadPercent = fromIntegral newTotalGramms / fromIntegral totalGrammsBegin

burnoutPlotPerson :: [Person] -> Ration -> [CarryItem] -> Person -> [(Int, Double)]
burnoutPlotPerson persons ration solvedCarryList person = 
  let (points, _, _) = foldl go ([], totalGrammsBegin, plist) $ zip [1..] meals
  in points
  where
    totalGrammsBegin = sum $ productGramm <$> plist
    meals = concat $ dayProducts <$> replacedRation person ration 
    
    plist :: [Product]
    plist = fmap toUnboxed $ concat $ fmap carryProducts
      $ filter ((Just person ==).carryPerson) solvedCarryList
    
    go :: ([(Int, Double)], Int, [Product]) -> (Int, [Product]) -> ([(Int, Double)], Int, [Product])
    go (points, totalGramms, carying) (i, meal) = (points ++ [(i, loadPercent)], newTotalGramms, newCarying)
      where
        mealProducts = (\p -> productSetGramm (productGramm p * personsWhoEat persons p) p) <$> meal
        (eaten, newCarying) = substractProds mealProducts carying
        newTotalGramms = totalGramms - eaten
        loadPercent = fromIntegral newTotalGramms / fromIntegral totalGrammsBegin
        
        substractProd :: Product -> [Product] -> (Int, [Product])
        substractProd prod boxedProds
          | not $ prod `elem` boxedProds = (0, boxedProds)
          | otherwise = let
            prod' = prod { productGramm = - productGramm prod}
            (Just boxedProd, newBoxedProds) = fetchOne prod boxedProds
            newBoxedProd = productAddGramm (productGramm prod') boxedProd
            in if productGramm newBoxedProd > 0 
               then (productGramm prod, newBoxedProd:newBoxedProds)
               else (productGramm prod, newBoxedProds)

        substractProds :: [Product] -> [Product] -> (Int, [Product])
        substractProds prods boxedProds = foldl go (0, boxedProds) prods
          where
            go :: (Int, [Product]) -> Product -> (Int, [Product])
            go (mass, boxedProds) prod = (mass + mass', newBoxedProds)
              where (mass', newBoxedProds) = substractProd prod boxedProds
                
dumpBurnoutPlot :: [Person] -> Ration -> [CarryItem] -> IO ()
dumpBurnoutPlot persons ration solvedCarryList = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    points, personsPoints :: [[String]]
    points = printPoint <$> burnoutPlot persons ration
    personsPoints = mergeManyColumns $ fmap (\(x, y) -> [show y]) . burnoutPlotPerson persons ration solvedCarryList <$> persons

    allPoints :: [[String]]
    allPoints = mergeColumns points personsPoints
    
    csvTable :: [[String]]
    csvTable = [["Прием", "Идеально"] ++ (personName <$> persons)]
      ++ allPoints

    printPoint :: (Int, Double) -> [String]
    printPoint (x, y) = [show x, show y]

dumpBurnoutPlot' :: [Person] -> Ration -> IO ()
dumpBurnoutPlot' persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    points = burnoutPlot persons ration
    
    csvTable :: [[String]]
    csvTable = [["Прием", "Доля"]]
      ++ (printPoint <$> points)

    printPoint :: (Int, Double) -> [String]
    printPoint (x, y) = [show x, show y]