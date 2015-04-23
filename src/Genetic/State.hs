module Genetic.State where

import Control.Applicative
import Genetic.Population
import System.Random

data GeneticState a = GeneticState {
  geneticFinished :: Bool,
  geneticCurrentGen :: Int,
  geneticPopulations :: [Population a],
  geneticCurrentBest :: Maybe (Double, a),
  geneticGen :: StdGen
}

initialGeneticState :: IO (GeneticState a)
initialGeneticState = GeneticState False 0 [] Nothing <$> newStdGen

isGeneticFinished :: GeneticState a -> Bool 
isGeneticFinished = geneticFinished
