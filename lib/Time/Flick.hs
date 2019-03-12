-- | A simple implementation of flicks, using 64-bit integers.
-- See https://github.com/OculusVR/Flicks#README for the spec.
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module Time.Flick where

import Control.Concurrent (threadDelay)
import Data.Ratio

import System.Clock

-- Time measured in units of flicks. One flick is precisely
-- @1/705600000@ seconds. Many common frame rates produce
-- frame times which are an integer number of flicks.
newtype Flicks = Flicks { unFlicks :: Integer }
  deriving newtype (Eq, Ord, Show, Num, Enum, Integral, Real)

-- | How many flicks are in a second. Precisely 705600000.
flicksPerSecond :: Num a => a
flicksPerSecond = 705600000

-- | One second in flicks.
secnd :: Flicks
secnd = Flicks flicksPerSecond

-- | Convert a number of seconds into flicks, rounding towards zero.
approxFlicks :: Rational -> Flicks
approxFlicks t = Flicks . truncate $ t * flicksPerSecond

-- | Duration in flicks of one oscillation at the given frequency in Hertz.
periodForFreq :: Rational -> Flicks
periodForFreq = approxFlicks . recip

-- | Get the current time in flicks.
flicksNow :: Clock -> IO Flicks
flicksNow clockTy = do
  TimeSpec{ sec, nsec } <- getTime clockTy
  pure (approxFlicks $ fromIntegral sec + fromIntegral nsec % 10^9)

-- | Suspend the current thread for a given duration.
-- Inherits 'threadDelay' 's lack of guarantees.
threadDelayFlicks :: Flicks -> IO ()
threadDelayFlicks (Flicks t) = do
  let micros = truncate (10^6 * t % flicksPerSecond)
  threadDelay micros