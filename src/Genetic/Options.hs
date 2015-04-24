{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Genetic.Options where

import Data.Typeable
import Data.Maybe
import Control.Applicative

data GeneticOptions = GeneticOptions {
  mutationChance :: Double,
  elitePart :: Double,
  maxGeneration :: Int,
  popCount :: Int,
  indCount :: Int,
  targetFitness :: Maybe Double
} deriving (Typeable, Show)

initialOptions :: GeneticOptions
initialOptions = GeneticOptions {
    popCount = 1,
    indCount = 50,
    maxGeneration = 10,
    targetFitness = Nothing,
    mutationChance = 0.8,
    elitePart = 0.1
  }
