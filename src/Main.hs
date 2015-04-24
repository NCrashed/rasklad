-- | Main entry point to the application.
module Main where

import Types
import Products
import Menu
import Persons
import Algorithms.Menu
import Algorithms.ProductList
import Algorithms.Purchase
import Algorithms.Carry.List
import Algorithms.Carry.Burnout
import Algorithms.Carry.Genetic

import Data.List
import Data.Maybe

-- Замечания:
-- Добавить в 2 рандомных дня банки свинной тушенки
-- перекус с поезда пожирнее (в этот день еще будет ужин)
-- второй день, подготовка к сухому обеду
-- разгрузить в первый ужин Бориса

-- | The main entry point.
main :: IO ()
main = do
  --dumpMenu menuCycle
  --dumpMenu fullMenu
  --dumpMenu $ replacedRation (findPerson "Кристина") fullMenu
  --dumpProductList persons fullMenu
  --dumpProductList' persons fullMenu
  --dumpPurchasesInteractive persons fullMenu
  --dumpPurchases persons fullMenu
  --dumpCarryList persons fullMenu
  --dumpBurnoutPlot persons fullMenu $ carryList persons $ totalProductList persons fullMenu

  --distr <- geneticSolve persons fullMenu
  --dumpBurnoutPlot persons fullMenu distr
  --dumpCarryList persons fullMenu $ mergeCarryList distr

  --dumpTemplateForDistr persons fullMenu
  dumpMenu' persons fullMenu
  
  --let tps = concat $ concat $ fmap dayProducts $ replacedRation (findPerson "Кристина") fullMenu
  --print $ sum $ fmap productGramm $ filter (("доп. сыр (Кристина)"==).productName) tps


findPerson :: String -> Person
findPerson name = fromJust $ find ((name==).personName) persons