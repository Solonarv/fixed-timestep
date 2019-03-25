-- | A simple implementation of flicks, using 64-bit integers.
-- See https://github.com/OculusVR/Flicks#README for the spec.
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Time.Flick
  ( -- * The Flicks data type
    Flicks(..)
  , flicksPerSecond, oneSecond
    -- * Conversions
  , approxFlicks
  , periodForFreq
  , timeSpecToFlicks
  , flicksToDiffTime
    -- * Measuring time in flicks
  , flicksNow
  , threadDelayFlicks
  ) where

import Control.Concurrent (threadDelay)
import Data.Fixed
import Data.Int
import Data.Proxy
import Data.Ratio

import Data.Time.Clock
import System.Clock

-- | Time measured in units of flicks. One flick is precisely
-- @1/705600000@ seconds. Many common frame rates produce
-- frame times which are an integer number of flicks.
newtype Flicks = Flicks { unFlicks :: Int64 }
  deriving newtype (Eq, Ord, Show, Num, Enum, Integral, Real)

-- | How many flicks are in a second: precisely 705600000.
flicksPerSecond :: Num a => a
flicksPerSecond = 705600000

-- | One second in flicks.
oneSecond :: Flicks
oneSecond = flicksPerSecond

-- | Convert a number of seconds into flicks, rounding towards zero.
approxFlicks :: Rational -> Flicks
approxFlicks t = Flicks . truncate $ t * flicksPerSecond

-- | Duration in flicks of one oscillation at the given frequency (in Hertz).
-- The result is rounded towards zero.
periodForFreq :: Rational -> Flicks
periodForFreq = approxFlicks . recip

-- | Convert a @'TimeSpec'@ into flicks. Rounds towards zero.
timeSpecToFlicks :: TimeSpec -> Flicks
timeSpecToFlicks TimeSpec{sec, nsec} = Flicks (secPart + nsecPart)
  where
    secPart = flicksPerSecond * sec
    nsecPart = (flicksPerSecond * nsec) `quot` 10^9

-- | Convert flicks into seconds with some amount of precision.
flicksToFixed :: forall r. HasResolution r => Flicks -> Fixed r
flicksToFixed (Flicks t) = MkFixed val
  where
    val = fromIntegral t * resolution (Proxy @r) `quot` flicksPerSecond

-- | Convert flicks into @'DiffTime'@
flicksToDiffTime :: Flicks -> DiffTime
flicksToDiffTime t = picosecondsToDiffTime picos
  where
    MkFixed picos = flicksToFixed t :: Fixed E12

-- | Get the current time in flicks. The epoch is an arbitrary starting
-- point and the maginute of the result should not be relied on.
-- 
-- See the documentation of @"System.Clock"@ for details.
flicksNow :: Clock -> IO Flicks
flicksNow clockTy = timeSpecToFlicks <$> getTime clockTy

-- | Suspend the current thread for a given duration.
-- Inherits 'threadDelay' 's lack of guarantees.
threadDelayFlicks :: Flicks -> IO ()
threadDelayFlicks (Flicks t) = do
  let micros = fromIntegral $ (10^6 * t) `quot` flicksPerSecond
  threadDelay micros