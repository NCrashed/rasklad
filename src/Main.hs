-- | Main entry point to the application.
module Main where

import Types
import Products
import Menu
import Persons
import Algorithms.Menu
import Algorithms.ProductList
import Algorithms.Purchase
import Algorithms.CarryList

-- Замечания:
-- Добавить в 2 рандомных дня банки свинной тушенки
-- перекус с поезда пожирнее (в этот день еще будет ужин)
-- второй день, подготовка к сухому обеду

-- | The main entry point.
main :: IO ()
main = do
  --dumpMenu menuCycle
  --dumpMenu fullMenu
  --dumpProductList persons fullMenu
  --dumpProductList' persons fullMenu
  --dumpPurchasesInteractive persons fullMenu
  --dumpPurchases persons fullMenu
  dumpCarryList' persons fullMenu