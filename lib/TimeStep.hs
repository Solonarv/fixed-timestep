-- | Utilities for repeatedly running @'IO'@ actions
-- at a specific frequency.
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module TimeStep
  ( -- * Repeating actions
    repeatedly
  , repeatedly'
    -- * Repeating actions in another thread
  , asyncRepeatedly
  , asyncRepeatedly'
    -- * Implementation details
  , TimeStepClock
  , newClock
  , loopUsing
    -- * Reexports (timekeeping)
  , module Time.Flick
  , Clock(..)
  ) where

import Control.Concurrent
import Control.Monad
import Data.Bifunctor
import Data.Fixed (divMod')
import Data.Int
import Data.IORef
import Data.Ratio

import Control.Concurrent.Async
import System.Clock

import Time.Flick

-- | Internal state of the stepper. Created using @'newClock'@.
data TimeStepClock = TimeStepClock
  { tscLastTick :: IORef Flicks
  , tscDesiredTickTime :: Flicks
  , tscClockType :: Clock
  }

-- | Run an action repeatedly at the specified frequency.
-- Uses the 'Monotonic' clock for timing.
repeatedly :: Rational -> IO a -> IO void
repeatedly freq = repeatedly' freq Monotonic

-- | Run an action repeatedly at the specified frequency,
-- using the given clock type.
repeatedly' :: Rational -> Clock -> IO a -> IO void
repeatedly' freq clockTy act = do
  clock <- newClock freq clockTy
  loopUsing clock act

-- | Run an action repeatedly at the specified frequency, in a separate thread.
-- Uses the 'Monotonic' clock for timing.
asyncRepeatedly :: Rational -> IO a -> IO (Async void)
asyncRepeatedly freq = asyncRepeatedly' freq Monotonic

-- | Run an action repeatedly at the specified frequency, in a separate thread,
-- using the given clock type.
asyncRepeatedly' :: Rational -> Clock -> IO a -> IO (Async void)
asyncRepeatedly' freq clockTy act = do
  clock <- newClock freq clockTy
  async (loopUsing clock act)

-- | Initialise a new clock structure for the given frequency and using
-- the given clock type.
newClock :: Rational -> Clock -> IO TimeStepClock
newClock freq clockTy = do
  leftoverRef <- newIORef =<< flicksNow clockTy
  pure TimeStepClock
    { tscLastTick = leftoverRef
    , tscDesiredTickTime = periodForFreq freq
    , tscClockType = clockTy
    }

-- | Repeat the given action using the information in the clock structure.
loopUsing :: TimeStepClock -> IO a -> IO void
loopUsing (TimeStepClock lastTickRef tickTime clockTy) act = forever $ do
  now <- flicksNow clockTy
  lastTick <- readIORef lastTickRef
  let elapsed = now - lastTick
      (ticksToRun, leftover) = elapsed `divMod` tickTime
  replicateM_ (fromIntegral ticksToRun) act
  writeIORef lastTickRef now
  threadDelayFlicks tickTime