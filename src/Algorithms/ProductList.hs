module Algorithms.ProductList where

import Types
import Products
import Persons
import Algorithms.Day
import Util

import Data.Csv
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

data BoxedProduct = BoxedProduct {
    boxedTimes :: Int
  , boxedProduct :: Product
} 

instance Show BoxedProduct where
  show bp = unwords [show $ boxedTimes bp, "x ", show $ boxedProduct bp]

instance Eq BoxedProduct where
  a == b = boxedProduct a == boxedProduct b
  
toBoxed :: Int -> Product -> BoxedProduct
toBoxed t p = BoxedProduct t (productSetGramm m p)
  where m = ceiling $ fromIntegral (productGramm p) / fromIntegral t

boxedSetGramm :: Int -> BoxedProduct -> BoxedProduct
boxedSetGramm m bp = bp { boxedProduct = productSetGramm m $ boxedProduct bp}

boxedGramm :: BoxedProduct -> Int
boxedGramm = productGramm . boxedProduct

boxedTotalGramm :: BoxedProduct -> Int
boxedTotalGramm bp = boxedTimes bp * boxedGramm bp

boxedAddTimes :: Int -> BoxedProduct -> BoxedProduct
boxedAddTimes i bp = bp { boxedTimes = boxedTimes bp + i }

boxedSetTimes :: Int -> BoxedProduct -> BoxedProduct
boxedSetTimes i bp = bp { boxedTimes = i }

mergeBoxed :: BoxedProduct -> BoxedProduct -> BoxedProduct
mergeBoxed p1 p2 = boxedAddTimes (boxedTimes p2) p1

mergeBoxed' :: BoxedProduct -> BoxedProduct -> BoxedProduct
mergeBoxed' p1 p2 = mergeBoxed p1 p2'
  where 
    m = boxedGramm p1
    t = boxedTotalGramm p2 `ceilDiv` m
    p2' = boxedSetTimes t (boxedSetGramm m p2) 

mergeBoxeds :: [BoxedProduct] -> [BoxedProduct]
mergeBoxeds ps = M.elems mp
  where 
    mp :: M.Map String BoxedProduct
    mp = foldr (\p -> M.insertWith mergeBoxed (productName $ boxedProduct p) p) M.empty ps

mergeBoxeds' :: [BoxedProduct] -> [BoxedProduct] -> [BoxedProduct]
mergeBoxeds' ps1 ps2 = M.elems $ M.unionWith mergeBoxed' (mp ps1) (mp ps2) 
  where 
    mp :: [BoxedProduct] -> M.Map String BoxedProduct
    mp ps = foldr (\p -> M.insertWith mergeBoxed (productName $ boxedProduct p) p) M.empty ps
    
class HasProduct a where
  getProduct :: a -> Product

instance HasProduct Product where
  getProduct = id

instance HasProduct BoxedProduct where
  getProduct = boxedProduct
  
productList :: [Person] -> Ration -> [BoxedProduct]
productList persons r = toBoxed' <$> (mergeProducts $ replaceMeat $ M.elems $ foldl collectDay M.empty r)
  where
    collectDay :: M.Map String Product -> RationDay -> M.Map String Product
    collectDay pmap day = pmap
      `mergeP` maybe M.empty collectMeal (breakfast day) 
      `mergeP` maybe M.empty collectMeal (dinner day) 
      `mergeP` maybe M.empty collectMeal (supper day) 

    collectMeal :: Meal -> M.Map String Product
    collectMeal m = M.fromList $ zip (productName <$> ps) ps
      where ps = mealProducts m
            
    mergeP = M.unionWith mergeProduct
    toBoxed' p = toBoxed (productOccurences r p) p
    
    replaceMeat :: [Product] -> [Product]
    replaceMeat ps = ps ++ personal
      where personal = concat $ (\p -> concat $ replaceProduct p <$> filter (needReplacement p) ps) <$> persons

productOccurences :: Ration -> Product -> Int
productOccurences r p 
  | productType p == Replacement = maybe 0 (length.replacedProducts) $ productTargetPerson p
  | otherwise = length $ filter (==p) products
  where products = concat $ concat.dayProducts <$> r

        meals :: [[Product]]
        meals = concat $ dayProducts <$> r

        replacedMeals :: Person -> [[Product]]
        replacedMeals per = mergeProducts . concat . fmap (replaceProduct per) . filter (needReplacement per) <$> meals

        replacedProducts :: Person -> [Product]
        replacedProducts = filter (p==) . concat . replacedMeals 
        
personsWhoEat :: HasProduct a => [Person] -> a -> Int
personsWhoEat ps p'
  | productType p == Replacement = length $ filter (\per -> isReplacement per p) ps
  | otherwise = length ps - length (filter (\per -> needReplacement per p ) ps)
  where
    p = getProduct p'
    
totalProductList :: [Person] -> Ration -> [BoxedProduct]
totalProductList ps r = (\p -> boxedSetGramm (boxedGramm p * personsWhoEat ps p) p) <$> plist
  where
    plist = productList ps r

dumpProductList' :: [Person] -> Ration -> IO ()
dumpProductList' ps r = do
  putStrLn "Список всех продуктов:"
  forM_ (productList ps r) print 

productPerPersonPerMeal :: Ration -> Product -> Int
productPerPersonPerMeal r p 
  | productType p == Replacement = maybe 0 (\person -> totalWeight (vRation person) p `ceilDiv` length (mealsThatHave (vRation person) p)) $ productTargetPerson p
  | otherwise = totalWeight r p `ceilDiv` length (mealsThatHave r p)
  where 
    vRation person = replacedRation person r
    
    meals :: Ration -> [[Product]]
    meals r' = concat $ dayProducts <$> r'

    mealsThatHave :: Ration -> Product -> [[Product]]
    mealsThatHave r' p = filter (p `elem`) $ meals r'

    totalWeight :: Ration -> Product -> Int
    totalWeight r' p = sum $ productGramm <$> (filter (==p) $ concat $ mealsThatHave r' p)
    
productPerPersonDay :: Ration -> Product -> Int
productPerPersonDay r p 
  | productType p == Replacement = maybe 0 (\person -> totalWeight (vRation person) p `ceilDiv` length (daysThatHave (vRation person) p)) $ productTargetPerson p
  | otherwise = totalWeight r p `ceilDiv` length (daysThatHave r p)
  where 
    vRation person = replacedRation person r
    
    days :: Ration -> [[Product]]
    days r' = concat.dayProducts <$> r'

    daysThatHave :: Ration -> Product -> [[Product]]
    daysThatHave r' p = filter (p `elem`) $ days r'

    totalWeight :: Ration -> Product -> Int
    totalWeight r' p = sum $ productGramm <$> (filter (==p) $ concat $ daysThatHave r' p)
    
productGroupPerMeal :: [Person] -> Ration -> Product -> Int
productGroupPerMeal ps r p 
  | productType p == Replacement = productPerPersonPerMeal r p
  | otherwise = weightMeals `ceilDiv` length meals
  where
    meals = concat $ filter (p `elem`).dayProducts <$> r
    weightMeals = sum $ (\p -> personsWhoEat ps p * productGramm p) <$> (filter (==p) $ concat meals)
    
dumpProductList :: [Person] -> Ration -> IO ()
dumpProductList ps r = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    plist = productList ps r

    csvTable :: [[String]]
    csvTable = [header] 
      ++ (printProduct <$> plist)
      ++ [[],["Всего", show totalGrammsPerPerson, show totalGramms]]
      ++ [[]]
      ++ footer
      
    totalGrammsPerPerson = sum $ boxedTotalGramm <$> plist
    totalGramms = sum $ (\p -> boxedTotalGramm p * personsWhoEat ps p) <$> plist
        
    header = ["Продукт", "Раз встречается", "Грамм на человека (один прием)", "Грамм на человека (день)", "Грамм на группу (прием)", "Грамм всего на человека", "Грамм всего"]
    printProduct p' = [
        capitalize $ productName p
      , show $ productOccurences r p
      , show $ productPerPersonPerMeal r p
      , show $ productPerPersonDay r p
      , show $ productGroupPerMeal ps r p
      , show $ productOccurences r p * productGramm p
      , show $ productOccurences r p * productGramm p * personsWhoEat ps p]
      where p = boxedProduct p'
        
    footer = [
        ["Вес на парня: ", show weightPerMale],
        ["Вес на девушку: ", show $ genderWeightFactor Female * weightPerMale]
      ]
      
    weightPerMale = fromIntegral totalGramms / (fromIntegral (length males) + genderWeightFactor Female * fromIntegral (length females))
    males = filter (\p -> personGender p == Male) persons
    females = filter (\p -> personGender p == Female) persons