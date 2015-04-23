module Algorithms.Menu where

import Types
import Products
import Algorithms.Day
import Util

import Data.Csv
import Data.Maybe
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
        
dumpMenu :: Ration -> IO ()
dumpMenu ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvMenuCycle
  where
    breakfasts = breakfast <$> ration
    dinners = dinner <$> ration
    suppers = supper <$> ration

    printMeal :: Meal -> [[String]]
    printMeal m = [[show $ mealTime m, ""]]++((\p -> [capitalize $ productName p, show $ productGramm p]) <$> mealProducts m)
    
    header = concat $ (\d -> ["День " ++ show (dayNumber d), ""]) <$> ration
    underSum = concat $ (\d -> ["Всего грамм: ", show (totalGrammsPerDay d)]) <$> ration
    
    csvMenuCycle :: [[String]]
    csvMenuCycle = [header]
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", ""]] printMeal <$> breakfasts) 
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", ""]] printMeal <$> dinners)
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", ""]] printMeal <$> suppers)
      ++ [[""]]
      ++ [underSum]
