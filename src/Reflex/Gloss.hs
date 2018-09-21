{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecursiveDo       #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Reflex.Gloss
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- A Gloss interface for Reflex.
--
-------------------------------------------------------------------------------

module Reflex.Gloss
  ( playReflex
  , InputEvent
  , GlossApp )
  where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (newTVarIO, writeTVar, readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Identity
import           Control.Monad.IO.Class (liftIO)
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.IORef             (readIORef)


import           Graphics.Gloss
  (Color, Display, Picture, blank)
import           Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G

import           Reflex
import           Reflex.Host.Class
  (newEventWithTriggerRef, runHostFrame, fireEvents, fireEventRef)
import           Reflex.Host.Basic

-- | Synonym for a Gloss Event to prevent name clashes.
type InputEvent = G.Event

-- | Convert the refresh and input events to a Behavior t Picture.
type GlossApp t m
  = BasicGuestConstraints t m
  => Event t Float
  -> Event t InputEvent
  -> BasicGuest t m (Dynamic t Picture)
-- | Play the 'GlossApp' in a window, updating when the Behavior t Picture
--   changes.
playReflex
  :: Display                    -- ^ Display mode.
  -> Color                      -- ^ Background color.
  -> Int                        -- ^ Maximum frames per second.
  -> (forall t m. GlossApp t m) -- ^ A reflex function that returns a 'Behavior t Picture'
  -> IO ()
playReflex display color frequency network =
  basicHostForever $ do
    picMVar <- liftIO $ newTVarIO blank
    timeMVar <- liftIO $ newTVarIO 0.0

    (tickEvent, tickTrigger)  <- newTriggerEvent
    (inputEvent, inputTrigger) <- newTriggerEvent

    dPicture <- network tickEvent inputEvent

    performEvent_ $
      liftIO . atomically . writeTVar picMVar <$>
      updated dPicture

    liftIO . void . forkIO $
      playIO display
      color
      frequency
      ()
      (\_ -> readTVarIO picMVar)
      (\ge _ -> inputTrigger ge)
      (\fl _ -> tickTrigger fl)
