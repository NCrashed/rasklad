{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Algorithms.Carry.Genetic where

import Types
import Algorithms.Carry.List
import Algorithms.Carry.Burnout

import Genetic.Coroutine 
import Genetic.Individ
import Genetic.State 
import Genetic.Population 

import Control.Applicative
import Control.Monad 
import Control.Monad.Random 
import GHC.Generics (Generic)
import Control.DeepSeq

newtype CarryDistrIndivid = CarryDistrIndivid [CarryItem]
  deriving (Show, Eq, Generic)

instance NFData CarryDistrIndivid

instance Individ CarryDistrIndivid where
  type IndividOptions CarryDistrIndivid = ([Person], Ration)

  crossover _ ia@(CarryDistrIndivid a) ib@(CarryDistrIndivid b) = undefined
  mutation _ ind@(CarryDistrIndivid a) = undefined
  initIndivid _ = undefined

fitness :: [Person] -> Ration -> CarryDistrIndivid -> Double
fitness persons ration (CarryDistrIndivid cl) = 1 / fromIntegral totalError
  where 
    idealPlot = burnoutPlot persons ration
    personPlots = burnoutPlotPerson persons ration cl <$> persons
    totalError = sum $ squareError idealPlot <$> personPlots
    
    squareError :: [(Int, Int)] -> [(Int, Int)] -> Int
    squareError as bs = sum $ (\((_, a), (_, b)) -> (a - b)*(a - b)) <$> zip as bs