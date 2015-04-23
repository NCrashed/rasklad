module Algorithms.Purchase where

import Types
import Products
import Algorithms.ProductList
import Util

import Prelude hiding (product)
import Data.Csv
import Data.List hiding (product)
import Data.Maybe
import Data.Either
import Data.Function
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS

data PurchaseBasket = PurchaseBasket {
    basketPerson :: Person,
    basketProducts :: [Either Product BoxedProduct]
} deriving Show

eitherProductName :: Either Product BoxedProduct -> String
eitherProductName (Left p) = productName p
eitherProductName (Right p) = productName $ boxedProduct p

initBaskets :: [Person] -> [PurchaseBasket]
initBaskets = fmap (\p -> PurchaseBasket p [])

predefinedProducts :: [PurchaseBasket] -> [PurchaseBasket]
predefinedProducts baskets = [
    addToBasket "Левон" [
      b 7 dryMilk
    , b 2 dryMilkMuesli
    , b 9 dryOnionCarrot
    , b 2 porkStew
    , b 2 tomatoPaste
    , b 2 raisins
    , b 2 kozinaki
    , b 2 millet
    , b 5 rusk
    , b 3 sausage
    ]
  , addToBasket "Гуща" [
      b 2 muesli
    , b 8 soup
    , b 8 zazharku
    , b 2 oatmealCoockies
    , sb 100 teaBlack
    , sb 100 teaBlackOrange
    , sb 100 teaGreen
    , b 2 lentil
    , b 4 pemmikan
    , b 5 rusk
    , b 3 sausage
    ]
  , addToBasket "Анатолий" [
      b 2 pasta
    , b 2 oatmeal
    , b 8 vermicelli
    , b 5 garlic
    , b 3 rice
    , b 4 rusk
    ]
  , addToBasket "Ольга" [
      b 2 vienneseWafers
    , b 5 dutchWaffles
    , b 2 dryCherry
    , b 4 nougat
    , b 8 porkPate
    , b 4 turkishDelight
    , b 2 spelled
    , b 3 pemmikan
    , b 4 rusk
    , b 3 prunes
    ]
  , addToBasket "Кристина" [
      byName 23 "доп. орехи (Кристина)"
    , byName 23 "доп. сыр (Кристина)"
    , b 3 coockiesChocolate
    , b 4 onion
    , b 3 buckwheat
    , b 2 halvaChocolate
    , sb 360 ghee
    ]
  , addToBasket "Борис" [
      b 3 sausage
    , b 2 tulaGingerbread
    , b 4 pemmikanSoup
    , sb 500 sugar
    , sb 500 sugarKasha
    , b 4 rusk
    ]
  , addToBasket "Япл Захарий Адам" [
    
    ]
  , addToBasket "Хитеш" [
      byName 17 "доп. орехи (Хитеш)"
    , byName 17 "доп. сыр (Хитеш)"
    ]
  , addToBasket "Святослав" [
      b 4 rusk
    , b 9 cheese
    , b 4 pemmikanSoup
    ]
  ]
  where
    b i p = Right $ toBoxed i p
    sb i p = Left $ productSetGramm i p
    
    addToBasket :: String -> [Either Product BoxedProduct] -> PurchaseBasket
    addToBasket name ps = basket { basketProducts = basketProducts basket ++ ps }
      where basket = findBasket name

    findBasket :: String -> PurchaseBasket 
    findBasket name = fromJust $ find ((name ==).personName.basketPerson) baskets

    byName :: Int -> String -> Either Product BoxedProduct
    byName i name = Right $ toBoxed i $ product name 0
    
recalcBaskets :: [BoxedProduct] -> [PurchaseBasket] -> [PurchaseBasket]
recalcBaskets totalProducts baskets = recalcBasket <$> baskets
  where 
    productOccurence :: BoxedProduct -> Int
    productOccurence p = length $ filter ((eitherProductName (Right p) ==).eitherProductName) $ concat $ basketProducts <$> baskets
        
    productAvgWeight :: BoxedProduct -> Int
    productAvgWeight p = avg $ boxedGramm <$> (filter (p==) totalProducts)

    productTotalWeight :: Product -> Int
    productTotalWeight p = sum $ boxedTotalGramm <$> (filter ((p==).boxedProduct) totalProducts)
    
    recalcBasket :: PurchaseBasket -> PurchaseBasket
    recalcBasket b = foldl recalcProduct b $ basketProducts b

    recalcProduct :: PurchaseBasket -> Either Product BoxedProduct -> PurchaseBasket
    recalcProduct b pr' = b { 
      basketProducts = replaceElem ((eitherProductName pr' ==).eitherProductName) updateProduct $ basketProducts b
      }
      where 
        updateProduct (Right p) = Right $ boxedSetGramm (productAvgWeight p) p 
        updateProduct (Left p) = Right $ toBoxed times $ productSetGramm (productGramm p * times) p
          where times = productTotalWeight p `ceilDiv` productGramm p `ceilDiv` occurs
                occurs = length $ filter (p==) $ lefts $ basketProducts b
                
remainedProductsToPurchase :: [BoxedProduct] -> [PurchaseBasket] -> [BoxedProduct]
remainedProductsToPurchase plist baskets = filterTimes $ mergeBoxeds' plist productsInBaskets
  where
    negateTimes p = p {boxedTimes = - boxedTimes p}
    filterTimes = filter (\p -> boxedTimes p /= 0)
    productsInBaskets = negateTimes <$> (rights.concat $ basketProducts <$> baskets)

dumpPurchasesInteractive :: [Person] -> Ration -> IO ()
dumpPurchasesInteractive persons ration = do
  putStrLn "Осталось распределить:"
  putStrLn $ unlines $ show <$> remained
  where
    baskets = recalcBaskets plist $ predefinedProducts $ initBaskets persons
    plist = totalProductList persons ration
    remained = remainedProductsToPurchase plist baskets

dumpPurchases :: [Person] -> Ration -> IO ()
dumpPurchases persons ration = BS.putStrLn $ encodeWith (defaultEncodeOptions {encUseCrLf = False}) $ csvTable
  where
    baskets = recalcBaskets plist $ predefinedProducts $ initBaskets persons
    plist = totalProductList persons ration
    rightsBasketProducts b= rights $ basketProducts b
    
    csvTable :: [[String]]
    csvTable = 
         (foldr mergeColumns [] $ printBasket <$> baskets)
      ++ (foldr mergeColumns [] $ printBasketInfo <$> baskets)

    printBasket :: PurchaseBasket -> [[String]]
    printBasket b = [[personName $ basketPerson b, "", ""]] 
      ++ [["Продукт", "Фасовка", "грамм"]]
      ++ (printProduct <$> rightsBasketProducts b)

    printBasketInfo :: PurchaseBasket -> [[String]] 
    printBasketInfo b = 
         [["Всего вес", show $ sum $ boxedTotalGramm <$> rightsBasketProducts b, ""]]
      ++ [["", "рублей", ""]]
      ++ [["Оценка трат:", "", ""]]
      ++ [["Потратил:", "", ""]]
      ++ [["Должен обществу:", "", ""]]
    
    printProduct :: BoxedProduct -> [String]
    printProduct p = [
        capitalize $ productName $ boxedProduct p
      , show $ boxedTimes p
      , show $ boxedTotalGramm p]