module Menu where

import Types
import Products
import Util

import Data.Maybe
import Data.List
import Control.Applicative

menuCycle :: Ration
menuCycle = [day1, day2, day3, day4]

fullMenu :: Ration
fullMenu = 
    vanishMeal 0 Breakfast
  $ vanishMeal 0 Dinner
  $ addProduct 2 Supper porkStew
  $ addProduct 6 Supper porkStew
  $ removeProduct 2 Supper pemmikan
  $ removeProduct 6 Supper pemmikan
  $ vanishMeal 9 Dinner
  $ vanishMeal 9 Supper
  $ makeFullMenu 10 menuCycle

makeFullMenu :: Int -> Ration -> Ration
makeFullMenu n = updateNumbers . take n . cycle
  where
    updateNumbers ds = (\(i, d) -> d {dayNumber = i}) <$> zip [1..] ds
    
vanishMeal :: Int -> MealTime -> Ration -> Ration
vanishMeal i mt r = replaceMeal i mt Nothing r

replaceMeal :: Int -> MealTime -> Maybe Meal -> Ration -> Ration
replaceMeal i mt m' r 
  | i > length r || i < 0 = r
  | otherwise = replace i md' r
  where
    md = r !! i
    md' = case mt of
      Breakfast -> md { breakfast = m' }
      Dinner -> md { dinner = m' }
      Supper -> md { supper = m' }

addProduct :: Int -> MealTime -> Product -> Ration -> Ration
addProduct i mt p r
  | i > length r || i < 0 = r
  | otherwise = replace i md' r
  where
    md = r !! i
    addMealProduct' = maybe Nothing (Just . addMealProduct p)
    md'= case mt of
      Breakfast -> md { breakfast = addMealProduct' $ breakfast md }
      Dinner -> md { dinner = addMealProduct' $ dinner md }
      Supper -> md { supper = addMealProduct' $ supper md }

removeProduct :: Int -> MealTime -> Product -> Ration -> Ration
removeProduct i mt p r
  | i > length r || i < 0 = r
  | otherwise = replace i md' r
  where
    md = r !! i
    removeMealProduct' = maybe Nothing (Just . removeMealProduct p)
    md'= case mt of
      Breakfast -> md { breakfast = removeMealProduct' $ breakfast md }
      Dinner -> md { dinner = removeMealProduct' $ dinner md }
      Supper -> md { supper = removeMealProduct' $ supper md }
      
addMealProduct :: Product -> Meal -> Meal
addMealProduct p m 
  | not (p `elem` mealProducts m) = m { mealProducts = mealProducts m ++ [p] }
  | otherwise = m { mealProducts = replace i (mergeProduct p p') $ mealProducts m }
  where
    i = fromJust $ p `elemIndex` mealProducts m
    p' = mealProducts m !! i

removeMealProduct :: Product -> Meal -> Meal
removeMealProduct p m 
  | not (p `elem` mealProducts m) = m
  | otherwise = m { mealProducts = removeItem i $ mealProducts m }
  where
    i = fromJust $ p `elemIndex` mealProducts m
    
day1 :: RationDay
day1 = RationDay 1 (Just br) (Just din) (Just sup)
  where
    br = Meal Breakfast [
        oatmeal
      , dryMilk
      , raisins
      , nougat
      , ghee
      , teaBlack
      , rusk
      , porkPate
      , sugar
      , sugarKasha
      ]
    din = Meal Dinner [
        soup
      , vermicelli
      , pemmikanSoup
      , zazharku
      , cheese
      , turkishDelight
      , rusk
      , sugar
      , teaGreen
      ]
    sup = Meal Supper [
        buckwheat
      , pemmikan
      , dryOnionCarrot
      , garlic
      , dutchWaffles
      , ghee
      , teaBlack
      , rusk
      , sausage
      , sugar
      ]

day2 :: RationDay
day2 = RationDay 2 (Just br) (Just din) (Just sup)
  where
    br = Meal Breakfast [
        rice
      , dryMilk
      , prunes
      , coockiesChocolate
      , ghee
      , teaBlack
      , rusk
      , cheese
      , sugar
      , sugarKasha
      ]
    din = Meal Dinner [
        soup
      , vermicelli
      , pemmikanSoup
      , zazharku
      , sausage
      , kozinaki
      , rusk
      , sugar
      , teaBlackOrange
      ]
    sup = Meal Supper [
        pasta
      , pemmikan
      , dryOnionCarrot
      , onion
      , tulaGingerbread
      , ghee
      , teaBlack
      , rusk
      , porkPate
      , sugar
      ]

day3 :: RationDay
day3 = RationDay 3 (Just br) (Just din) (Just sup)
  where
    br = Meal Breakfast [
        muesli
      , dryMilkMuesli
      , vienneseWafers
      , teaBlack
      , rusk
      , sausage
      , sugar
      , sugarKasha
      ]
    din = Meal Dinner [
        soup
      , vermicelli
      , pemmikanSoup
      , zazharku
      , porkPate
      , oatmealCoockies
      , rusk
      , sugar
      , teaGreen
      ]
    sup = Meal Supper [
        lentil
      , pemmikan
      , dryOnionCarrot
      , garlic
      , tomatoPaste
      , turkishDelight
      , ghee
      , teaBlack
      , rusk
      , cheese
      , sugar
      ] 

day4 :: RationDay
day4 = RationDay 4 (Just br) (Just din) (Just sup)
  where
    br = Meal Breakfast [
        millet
      , dryMilk
      , ghee
      , dryCherry
      , dutchWaffles
      , teaBlack
      , rusk
      , porkPate
      , sugar
      , sugarKasha
      ]
    din = Meal Dinner [
        soup
      , vermicelli
      , pemmikanSoup
      , zazharku
      , cheese
      , nougat
      , rusk
      , sugar
      , teaBlackOrange
      ]
    sup = Meal Supper [
        spelled
      , pemmikan
      , dryOnionCarrot
      , onion
      , halvaChocolate
      , ghee
      , teaBlack
      , rusk
      , sausage
      , sugar
      ]