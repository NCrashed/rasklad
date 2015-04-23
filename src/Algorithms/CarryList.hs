module Algorithms.CarryList where

import Types
import Products
import Algorithms.ProductList
import Util

import Control.Applicative
import Control.Arrow
import Control.Monad

data CarryItem = CarryItem {
    carryProducts :: [BoxedProduct]
  , carryPerson :: Maybe Person
} deriving (Show)

toCarryItem :: [BoxedProduct] -> CarryItem
toCarryItem bs = CarryItem bs Nothing

toCarryItem' :: BoxedProduct -> CarryItem
toCarryItem' b = CarryItem [b] Nothing

carryAssignPerson :: Person -> CarryItem -> CarryItem
carryAssignPerson p it = it { carryPerson = Just p }

carryGramm :: CarryItem -> Int
carryGramm = sum . fmap boxedTotalGramm . carryProducts

type CarryRule = [BoxedProduct] -> (Maybe CarryItem, [BoxedProduct])

carryList :: [Person] -> [BoxedProduct] -> [CarryItem]
carryList persons bs = fst applyRules ++ (toCarryItem' <$> snd applyRules)
  where
    applyRules :: ([CarryItem], [BoxedProduct])
    applyRules = foldl applyRule ([], bs) $ carryRules persons

    applyRule :: ([CarryItem], [BoxedProduct]) -> CarryRule -> ([CarryItem], [BoxedProduct])
    applyRule (processed, next) rule = let (mi, left) = rule next in (maybe processed (:processed) mi, left)


carryRules :: [Person] -> [CarryRule]
carryRules persons = concat [
    [ mkClusterRule [sugar, sugarKasha]
    , mkClusterRule [soup, zazharku]
    , mkClusterRule [teaBlack, teaBlackOrange, teaGreen]
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
    plist = totalProductList persons r
    clist = carryList persons plist