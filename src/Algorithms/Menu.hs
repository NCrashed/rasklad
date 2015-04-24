module Algorithms.Menu where

import Types
import Products
import Algorithms.Day
import Algorithms.ProductList
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

dumpMenu' :: [Person] -> Ration -> IO ()
dumpMenu' persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvMenuCycle
  where
    breakfasts = fmap mealWithReplacements . breakfast <$> ration
    dinners = fmap mealWithReplacements . dinner <$> ration
    suppers = fmap mealWithReplacements . supper <$> ration

    mealWithReplacements :: Meal -> Meal
    mealWithReplacements meal = meal { mealProducts = newMealProducts }
      where
        newMealProducts = mergeProducts $ concat $ repl <$> mealProducts meal
        repl pr = pr : (concat $ (\person -> if needReplacement person pr then replaceProduct person pr else []) <$> persons)
          
    printMeal :: Meal -> [[String]]
    printMeal m = [[show $ mealTime m, "Грамм чел.", "Грамм группа", "Несет"]]++((\p -> [
      capitalize $ productName p, 
      show $ productGramm p,
      show $ productGramm p * personsWhoEat persons p
      ]) <$> mealProducts m)
    
    header = concat $ (\d -> ["День " ++ show (dayNumber d), "", "", ""]) <$> ration
    
    csvMenuCycle :: [[String]]
    csvMenuCycle = [header]
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", "", "", ""]] printMeal <$> breakfasts) 
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", "", "", ""]] printMeal <$> dinners)
      ++ [[""]]
      ++ (foldr mergeColumns [] $ maybe [["", "", "", ""]] printMeal <$> suppers)