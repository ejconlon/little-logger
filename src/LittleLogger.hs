{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

-- | Basic logging based on co-log. Meant to give you what you need to get started with a single module import.
-- The root module exports 'LittleLogger.Reader' for use with RIO-style stacks, but if you want to manually
-- pass a 'SimpleLogAction' around, import 'LittleLogger.Manual' instead.
module LittleLogger
  ( module LittleLogger.Reader
  ) where

import LittleLogger.Reader
