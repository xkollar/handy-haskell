{-# LANGUAGE CPP #-}
module FormatTime (formatISO8601) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
-- #if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
-- #else
-- import System.Locale (defaultTimeLocale)
-- #endif


formatISO8601 :: UTCTime -> String
formatISO8601 t = formatTime defaultTimeLocale "%FT%TZ" t

