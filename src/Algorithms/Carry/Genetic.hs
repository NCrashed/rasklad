{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Algorithms.Carry.Genetic where

import Types
import Algorithms.Carry.List
import Algorithms.Carry.Burnout
import Algorithms.ProductList
import Algorithms.Purchase
import Util

import Genetic.Options
import Genetic.Solve 
import Genetic.State
import Genetic.Coroutine
import Genetic.Individ

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad 
import Control.Monad.Random 
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

newtype CarryDistrIndivid = CarryDistrIndivid [CarryItem]
  deriving (Show, Eq, Generic)

instance NFData CarryDistrIndivid

instance Individ CarryDistrIndivid where
  type IndividOptions CarryDistrIndivid = ([Person], Ration)

  crossover _ ia@(CarryDistrIndivid a) ib@(CarryDistrIndivid b)
    | length a /= length b = fail "Chromosomes have different lengths"
    | length a <= 1 = return (ia, ib)
    | length a == 2 = 
      let [a0, a1] = a 
          [b0, b1] = b
      in return (CarryDistrIndivid [a0, b1], CarryDistrIndivid [b0, a1])
    | otherwise = crossover3 ia ib
    
  mutation (persons, ration) ind@(CarryDistrIndivid a) = do
    i <- uniform slots
    p <- randomPerson
    return $! CarryDistrIndivid $ replace i ( (a !! i) { carryPerson = Just p } ) a
    where
      baseSolution = carryList persons $ purchaseProductList persons ration      
      randomPerson = uniform persons
      slotsStatus = isNothing.carryPerson <$> baseSolution
      slots = fmap fst $ filter (\(_, s) -> s) $ zip [0..] slotsStatus
      
  initIndivid (persons, ration) = CarryDistrIndivid <$> mapM assignRandPerson baseSolution
    where 
      baseSolution = carryList persons $ purchaseProductList persons ration
      randomPerson = uniform persons
      assignRandPerson ci = case carryPerson ci of
        Nothing -> do
          p <- randomPerson
          return $! ci { carryPerson = Just p }
        Just _ -> return $! ci
        
fitness :: [Person] -> Ration -> CarryDistrIndivid -> Double
fitness persons ration (CarryDistrIndivid cl) = 1 / totalError
  where 
    idealPlot = burnoutPlot persons ration
    personPlots = burnoutPlotPerson persons ration cl <$> persons
    totalError = sum $ squareError idealPlot <$> personPlots
    
    squareError :: [(Int, Double)] -> [(Int, Double)] -> Double
    squareError as bs = sum $ (\((_, a), (_, b)) -> (a - b)*(a - b)) <$> zip as bs

-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: CarryDistrIndivid -> CarryDistrIndivid -> PauseableRand (CarryDistrIndivid, CarryDistrIndivid)
crossover3 (CarryDistrIndivid a) (CarryDistrIndivid b) = do
  [p1, p2, p3] <- sort <$> replicateM 3 (getRandomR (1, n - 2))
  let a' = concat [slice 0 p1 a, slice p1 p2 b, slice p2 p3 a, slice p3 n b]
  let b' = concat [slice 0 p1 b, slice p1 p2 a, slice p2 p3 b, slice p3 n a]
  return $ (CarryDistrIndivid a', CarryDistrIndivid b')
  where
    n = length a
    slice i1 i2 = take (i2 - i1) . drop i1

geneticSolve :: [Person] -> Ration -> IO [CarryItem]
geneticSolve persons ration = do
  geneticState <- initialGeneticState
  loop geneticState Nothing
  where
    loop :: GeneticState CarryDistrIndivid -> Maybe (Pauseable (GeneticState CarryDistrIndivid)) -> IO [CarryItem]
    loop geneticState coroutine = do
      corRes <- case coroutine of
        Nothing -> resume $ solve (persons, ration) (fitness persons ration) options geneticState
        Just cr -> resume cr
      (newGeneticState, newCoroutine) <- case corRes of
        Left (Yield _ paused) -> return (geneticState, Just paused)
        Right genst -> return (genst, Nothing)
      case isGeneticFinished newGeneticState of
        False -> do
          let currentFitness = maybe 0 fst $ geneticCurrentBest newGeneticState
          putStrLn $ unwords [
              show $ geneticCurrentGen newGeneticState
            , show $ currentFitness
            ]
          loop newGeneticState newCoroutine
        True -> case geneticCurrentBest newGeneticState of
          Nothing -> fail "impossible"
          Just (_, CarryDistrIndivid solution) -> return solution
          
    options = initialOptions