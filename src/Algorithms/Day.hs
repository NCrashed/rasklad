module Algorithms.Day where

import Types

import Data.Maybe
import Control.Applicative

totalGrammsPerDay :: RationDay -> Int
totalGrammsPerDay d = 
  maybe 0 totalGrammsPerMeal (breakfast d)
  + maybe 0 totalGrammsPerMeal (dinner d)
  + maybe 0 totalGrammsPerMeal (supper d)

totalGrammsPerMeal :: Meal -> Int
totalGrammsPerMeal m = sum $ productGramm <$> mealProducts m