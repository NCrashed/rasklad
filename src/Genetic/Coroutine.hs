module Genetic.Coroutine where

import Control.Monad.Trans.Class
import Control.Monad.Random
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import System.Random

type GenRand = RandT StdGen 
type Pauseable = Coroutine (Yield ()) IO
type PauseableRand = GenRand Pauseable

pause :: PauseableRand ()
pause = lift $ yield ()
