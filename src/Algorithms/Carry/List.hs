{-# LANGUAGE TupleSections #-}
module Algorithms.Carry.List where

import Types
import Products
import Algorithms.ProductList
import Algorithms.Purchase
import Util

import Data.Csv
import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

data CarryItem = CarryItem {
    carryProducts :: [BoxedProduct]
  , carryPerson :: Maybe Person
} deriving (Show, Eq)

toCarryItem :: [BoxedProduct] -> CarryItem
toCarryItem bs = CarryItem bs Nothing

toCarryItem' :: BoxedProduct -> [CarryItem]
toCarryItem' b = replicate (boxedTimes b) $ CarryItem [b {boxedTimes = 1}] Nothing

carryAssignPerson :: Person -> CarryItem -> CarryItem
carryAssignPerson p it = it { carryPerson = Just p }

carryGramm :: CarryItem -> Int
carryGramm = sum . fmap boxedTotalGramm . carryProducts

type CarryRule = [BoxedProduct] -> (Maybe CarryItem, [BoxedProduct])

carryList :: [Person] -> [BoxedProduct] -> [CarryItem]
carryList persons bs = fst applyRules ++ (concat $ toCarryItem' <$> snd applyRules)
  where
    applyRules :: ([CarryItem], [BoxedProduct])
    applyRules = foldl applyRule ([], bs) $ carryRules persons

    applyRule :: ([CarryItem], [BoxedProduct]) -> CarryRule -> ([CarryItem], [BoxedProduct])
    applyRule (processed, next) rule = let (mi, left) = rule next in (maybe processed (:processed) mi, left)


carryRules :: [Person] -> [CarryRule]
carryRules persons = concat [
    [ mkClusterRule [soup, zazharku]
    , mkClusterRule [teaBlack, teaBlackOrange, teaGreen]
    --, mkClusterRule [sugar, sugarKasha]
    ]
  , replacementRule <$> persons
  ]
  where
    
    toCarry [] = Nothing
    toCarry is = Just $ toCarryItem is

    mkClusterRule :: [Product] -> CarryRule
    mkClusterRule cluster = \ps -> first toCarry $ fetchElements (toBoxed 1 <$> cluster) ps

    isReplacementProduct :: Person -> BoxedProduct -> Bool
    isReplacementProduct person bp = productType p == Replacement && maybe False (==person) (productTargetPerson p)
      where p = boxedProduct bp

    replacementRule :: Person -> CarryRule
    replacementRule person ps = first (fmap (carryAssignPerson person) . toCarry) $ fetchElements is ps
      where is = filter (isReplacementProduct person) ps

{-
distributeOverRation :: [Person] -> Ration -> [CarryItem] -> [CarryItem]
distributeOverRation persons ration carryItems' = undefined
  where
    carryItems = toCarryItemWithUsage <$> carryItems'
    rationCoords r = iterateOverRation r

    go :: ([CarryItemWithUsage], [CarryItemWithUsage]) -> MealCoord -> ([CarryItemWithUsage], [CarryItemWithUsage])
    go (processed, needProcessing) mc = case findCarry product needProcessing of
      (Nothing, _) -> (processed, needProcessing)
      (Just ci, newNeedProcessing) -> let
        ci' = 
      where
        Just product = getProductByCoord mc ration

        findCarry :: Product -> [CarryItemWithUsage] -> (Maybe CarryItemWithUsage, [CarryItemWithUsage])
        findCarry pr = fetchOne' f
          where 
            f :: CarryItemWithUsage -> Bool
            f ci = or $ fmap (\(bp, _, _) -> productName pr == productName (boxedProduct bp)) $ carryProducts' ci
        
data CarryItemWithUsage = CarryItemWithUsage {
    carryProducts' :: [(BoxedProduct, Int, MealCoord)]
  , carryPerson' :: Maybe Person
} deriving (Show, Eq)

toCarryItemWithUsage :: CarryItem -> CarryItemWithUsage
toCarryItemWithUsage ci = CarryItemWithUsage {
    carryProducts' = fmap (\(p, c) -> (p, 0, c)) $ carryProducts ci
  , carryPerson' = carryPerson ci
  }

fromCarryItemWithUsage :: CarryItemWithUsage -> CarryItem
fromCarryItemWithUsage ciu = CarryItem {
    carryProducts = fmap (\(p, _, c) -> (p, c)) $ carryProducts' ciu
  , carryPerson = carryPerson' ciu
  }
-}

mergeCarryList :: [CarryItem] -> [CarryItem]
mergeCarryList ci = M.elems $ foldl go M.empty ci
  where
    makeIndex :: CarryItem -> String
    makeIndex ci = concat (show <$> carryProducts ci) ++ show (carryPerson ci)
    
    go :: M.Map String CarryItem -> CarryItem -> M.Map String CarryItem
    go m ci = M.insertWith mergeCarryItem (makeIndex ci) ci m

    mergeCarryItem :: CarryItem -> CarryItem -> CarryItem
    mergeCarryItem ci _ = ci { carryProducts = carryProducts' }
      where 
        carryProducts' = boxedAddTimes 1 <$> carryProducts ci
        
dumpCarryList' :: [Person] -> Ration -> IO ()
dumpCarryList' persons r = do
  putStrLn "Кто-что несет:"
  forM_ clist print
  where
    plist = purchaseProductList persons r
    clist = carryList persons plist

dumpCarryList :: [Person] -> Ration -> [CarryItem] -> IO ()
dumpCarryList persons ration clist = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    --plist = purchaseProductList persons ration
    --clist = carryList persons plist

    csvTable :: [[String]]
    csvTable = [["Продукт", "Фасовка", "Граммы", "Несёт"]]
      ++ concat (printItem <$> clist)

    printItem :: CarryItem -> [[String]]
    printItem ci = mergeColumns ((printBody <$> carryProducts ci) ++ [[""]]) [[maybe "" personName $ carryPerson ci]]
      where 
        printBody :: BoxedProduct -> [String]
        printBody p = [
            capitalize $ productName $ boxedProduct p
          , show $ boxedTimes p
          , show $ boxedTotalGramm p
          ]

dumpTemplateForDistr :: [Person] -> Ration -> IO ()
dumpTemplateForDistr persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    plist = purchaseProductList persons ration
    clist = mergeCarryList $ carryList persons plist

    csvTable :: [[String]]
    csvTable = [["Продукт", "Фасовка", "Граммы удельно"] ++ (personName <$> persons)]
      ++ concat (printItem <$> clist)

    printItem :: CarryItem -> [[String]]
    printItem ci = (printBody <$> carryProducts ci) ++ [[""]]
      where 
        printBody :: BoxedProduct -> [String]
        printBody p = [
            capitalize $ productName $ boxedProduct p
          , show $ boxedTimes p
          , show $ boxedGramm p
          ]