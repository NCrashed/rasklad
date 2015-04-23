module Genetic.Solve where

import Data.Maybe
import Control.Monad 
import Control.Monad.Random
import Control.DeepSeq
import Control.Applicative

import Genetic.Coroutine
import Genetic.Individ
import Genetic.Population
import Genetic.State 
import Genetic.Options 


-- | Solving the problem using genetic algorithm
solve :: Individ a => IndividOptions a -> Fitness a -> GeneticOptions -> GeneticState a -> Pauseable (GeneticState a)
solve iopts fitness opts state 
  | isGeneticFinished state = return state
  | otherwise = do
    (newState, newGen) <- runRandT solve' $ geneticGen state
    return $ newState { geneticGen = newGen }
    where
      currGeneration = geneticCurrentGen state

      solve' = do
        pops <- if currGeneration == 0 
          then replicateM (popCount opts) $ pause >> initPopulation iopts (indCount opts)
          else return $ geneticPopulations state

        newPops <- pops `deepseq` mapM (\p -> pause >> nextPopulation iopts fitness opts p) pops
        
        let currBest = findBest fitness newPops
        return $ state {
          geneticFinished = maybe False (isFinished . fst) currBest,
          geneticCurrentGen = currGeneration + 1,
          geneticPopulations = newPops,
          geneticCurrentBest = currBest
        }

      isFinished fit = 
           currGeneration + 1 >= maxGeneration opts
        || (isJust (targetFitness opts) && fit >= fromJust (targetFitness opts))
