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
    
dumpCarryList' :: [Person] -> Ration -> IO ()
dumpCarryList' persons r = do
  putStrLn "Кто-что несет:"
  forM_ clist print
  where
    plist = purchaseProductList persons r
    clist = carryList persons plist

dumpCarryList :: [Person] -> Ration -> IO ()
dumpCarryList persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    plist = purchaseProductList persons ration
    clist = carryList persons plist

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