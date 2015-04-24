module Types where

import Data.Maybe
import Control.Applicative
import Util

data ProductType = Beef | Pork | Replacement | Other
  deriving (Eq, Ord, Enum)

instance Show ProductType where
  show Beef = "Содержит говядину"
  show Pork = "Содержит свинину"
  show Replacement = "Замена говядине/свинине"
  show Other = "Вегетарианская еда"
  
data Product = Product {
    productName :: String,
    productGramm :: Int,
    productType :: ProductType,
    productTargetPerson :: Maybe Person
}

product :: String -> Int -> Product
product s g = Product s g Other Nothing

productPork :: String -> Int -> Product
productPork s g = Product s g Pork Nothing

productBeef :: String -> Int -> Product
productBeef s g = Product s g Beef Nothing

productAddGramm :: Int -> Product -> Product
productAddGramm m p = p { productGramm = productGramm p + m }

productSetGramm :: Int -> Product -> Product
productSetGramm m p = p { productGramm = m }

instance Eq Product where
  p1 == p2 = productName p1 == productName p2

instance Show Product where
  show p = productName p ++ ": " ++ (show $ productGramm p) ++ " г, " ++ (show $ productType p)

data Gender = Male | Female
  deriving (Eq, Ord, Enum, Show)
  
data Person = Person {
    personName :: String
  , personFlavor :: ProductType
  , personGender :: Gender
} deriving Show

instance Eq Person where
  a == b = personName a == personName b
  
data MealTime = Breakfast | Dinner | Supper
  deriving (Eq, Ord, Enum)

instance Show MealTime where
  show Breakfast = "Завтрак"
  show Dinner = "Обед"
  show Supper = "Ужин"
  
data Meal = Meal {
    mealTime :: MealTime,
    mealProducts :: [Product]
}

data RationDay = RationDay {
    dayNumber :: Int,
    breakfast :: Maybe Meal,
    dinner :: Maybe Meal,
    supper :: Maybe Meal
}

dayProducts :: RationDay -> [[Product]]
dayProducts d = 
  [ dayMealProducts Breakfast d 
  , dayMealProducts Dinner d 
  , dayMealProducts Supper d]

dayMealProducts :: MealTime -> RationDay -> [Product]
dayMealProducts mt d = case mt of
  Breakfast -> maybe [] mealProducts $ breakfast d
  Dinner -> maybe [] mealProducts $ dinner d
  Supper -> maybe [] mealProducts $ supper d
  
type Ration = [RationDay]

data MealCoord = MealCoord {
    mealCoordDay :: Int
  , mealCoordTime :: MealTime
  , mealCoordPos :: Int
} deriving (Eq)

instance Show MealCoord where
  show mc = unwords [
      "День", show $ mealCoordDay mc
    , ":", show $ mealCoordTime mc
    , ", продукт №" ++ show (mealCoordPos mc)
    ]

zeroMealCoord :: MealCoord
zeroMealCoord = MealCoord 0 Breakfast 0

isCoordValid :: MealCoord -> Ration -> Bool
isCoordValid mc r = 
  mealCoordDay mc >= 0 && mealCoordDay mc < length r
  && isMealTimeValid day
  && isProductPosValid (mealProducts meal)
  where
    day = r !! mealCoordDay mc
    meal = case mealCoordTime mc of
      Breakfast -> fromJust $ breakfast day
      Dinner -> fromJust $ dinner day
      Supper -> fromJust $ supper day
    
    isMealTimeValid d = case mealCoordTime mc of
      Breakfast -> isJust $ breakfast d
      Dinner -> isJust $ dinner d
      Supper -> isJust $ supper d
      
    isProductPosValid m = let i = mealCoordPos mc
                              in i >= 0 && i < length m
    
getProductByCoord :: MealCoord -> Ration -> Maybe Product
getProductByCoord mc ration 
  | not $ isCoordValid mc ration = Nothing
  | otherwise = Just $ (mealProducts meal) !! mealCoordPos mc
  where
    day = ration !! mealCoordDay mc
    meal = case mealCoordTime mc of
      Breakfast -> fromJust $ breakfast day
      Dinner -> fromJust $ dinner day
      Supper -> fromJust $ supper day

setProductByCoord :: MealCoord -> Product -> Ration -> Ration
setProductByCoord mc prod ration
  | not $ isCoordValid mc ration = ration
  | otherwise = replace (mealCoordDay mc) newDay ration
  where
    day = ration !! mealCoordDay mc
    newDay = case mealCoordTime mc of
      Breakfast -> day { breakfast = Just newMeal}
      Dinner -> day { dinner = Just newMeal }
      Supper -> day { supper = Just newMeal }
      
    meal = case mealCoordTime mc of
      Breakfast -> fromJust $ breakfast day
      Dinner -> fromJust $ dinner day
      Supper -> fromJust $ supper day

    newMeal = meal { mealProducts = replace (mealCoordPos mc) prod $ mealProducts meal }
    
updateProductByCoord :: MealCoord -> (Product -> Product) -> Ration -> Ration
updateProductByCoord mc upd ration = case getProductByCoord mc ration of
  Nothing -> ration
  Just p -> setProductByCoord mc p ration
  
iterateOverRation :: Ration -> [MealCoord]
iterateOverRation ration = concat $ uncurry dayToCoords <$> zip [0..] ration
  where
    dayToCoords :: Int -> RationDay -> [MealCoord]
    dayToCoords i rd = 
         maybe [] (mealToCoords i Breakfast) (breakfast rd)
      ++ maybe [] (mealToCoords i Dinner) (dinner rd)
      ++ maybe [] (mealToCoords i Supper) (supper rd)

    mealToCoords :: Int -> MealTime -> Meal -> [MealCoord]
    mealToCoords i mt meal = MealCoord i mt <$> [0 .. length (mealProducts meal) - 1]