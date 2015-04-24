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
burnoutPlot :: [Person] -> Ration -> [(Int, Int)]
burnoutPlot persons ration = fst $ foldl go ([], sumGramms plist) $ zip [1..] meals
  where
    meals = concat $ dayProducts <$> ration
    plist = filter notReplacement $ totalProductList persons ration
    notReplacement p = productType (boxedProduct p) /= Replacement
    sumGramms pl = sum $ boxedTotalGramm <$> plist
    
    go :: ([(Int, Int)], Int) -> (Int, [Product]) -> ([(Int, Int)], Int)
    go (points, totalGramms) (i, meal) = (points ++ [(i, newTotalGramms)], newTotalGramms)
      where
        mealGramms = sum $ (\p -> productGramm p * personsWhoEat persons p) <$> meal
        newTotalGramms = totalGramms - mealGramms

burnoutPlotPerson :: [Person] -> Ration -> [CarryItem] -> Person -> [(Int, Int)]
burnoutPlotPerson persons ration solvedCarryList person = 
  let (points, _, _) = foldl go ([], sum $ productGramm <$> plist, plist) $ zip [1..] meals
  in points
  where
    meals = concat $ dayProducts <$> replacedRation person ration 
    
    plist :: [Product]
    plist = fmap toUnboxed $ concat $ fmap carryProducts
      $ filter ((Just person ==).carryPerson) solvedCarryList
    
    go :: ([(Int, Int)], Int, [Product]) -> (Int, [Product]) -> ([(Int, Int)], Int, [Product])
    go (points, totalGramms, carying) (i, meal) = (points ++ [(i, newTotalGramms)], newTotalGramms, newCarying)
      where
        mealProducts = (\p -> productSetGramm (productGramm p * personsWhoEat persons p) p) <$> meal
        (eaten, newCarying) = substractProds mealProducts carying
        newTotalGramms = totalGramms - eaten
        
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
    csvTable = [["Прием", "Граммы"] ++ (personName <$> persons)]
      ++ allPoints

    printPoint :: (Int, Int) -> [String]
    printPoint (x, y) = [show x, show y]

dumpBurnoutPlot' :: [Person] -> Ration -> IO ()
dumpBurnoutPlot' persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    points = burnoutPlot persons ration
    
    csvTable :: [[String]]
    csvTable = [["Прием", "Граммы"]]
      ++ (printPoint <$> points)

    printPoint :: (Int, Int) -> [String]
    printPoint (x, y) = [show x, show y]