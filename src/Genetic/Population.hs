module Genetic.Population where

import Data.Function
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Control.Monad 

import Genetic.Coroutine 
import Genetic.Individ
import Genetic.Options

type Population a = [a]
type Fitness a = a -> Double 

-- | Fetching best solution from populations      
findBest :: Individ a => Fitness a -> [Population a] -> Maybe (Double, a)
findBest fitness pops = case catMaybes $ (findPopBest fitness) <$> pops of
  [] -> Nothing
  bests -> Just $ maximumBy (compare `on` fst) bests
  

-- | Fetching best solution from population
findPopBest :: Individ a => Fitness a -> Population a -> Maybe (Double, a)
findPopBest fitness pop 
  | null pop = Nothing
  | otherwise = Just $ maximumBy (compare `on` fst) $ first fitness <$> zip pop pop

-- | Creating population with m chromosomes with length n
initPopulation :: Individ a => IndividOptions a -> Int -> PauseableRand (Population a)
initPopulation iopts n = replicateM n $ initIndivid iopts

-- | Helper to choose between two elements with provided chance of the first one
randChoice :: Rational -> PauseableRand a -> PauseableRand a -> PauseableRand a
randChoice chance th els = join (fromList [(th, chance), (els, 1 - chance)])

-- | Caclulates next generation of population
nextPopulation :: Individ a => IndividOptions a -> Fitness a -> GeneticOptions -> Population a -> PauseableRand (Population a)
nextPopulation iopts fitness opts pop = do
  newPop' <- liftM concat $ forM [1 .. ceiling $ fromIntegral nonEliteCount / 2] $ \i -> do
    when (i `mod` 25 == 0) pause
    a1 <- takeChr
    b1 <- takeChr
    (a2, b2) <- crossover iopts a1 b1
    a3 <- applyMutation a2
    b3 <- applyMutation b2
    return [a3, b3]
  let newPop = elite ++ newPop'
  return $ if length newPop <= length pop then newPop else init newPop
  where fits = toRational . fitness <$> pop
        maxfit = maximum fits
        chances = zip pop ((/maxfit) <$> fits)
        takeChr = fromList chances
        mutChance = toRational $ mutationChance opts
        applyMutation c = randChoice mutChance (mutation iopts c) (return c)
        bests = snd <$> sortBy (flip compare `on` fst) (first fitness <$> zip pop pop)
        elite = take (ceiling $ fromIntegral (length bests) * elitePart opts) bests
        nonEliteCount = length pop - length elite
