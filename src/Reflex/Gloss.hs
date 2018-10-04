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
  ( module Reflex.Gloss.Event
  , playReflex
  , GlossApp
  , BasicGuestConstraints
  , BasicGuest
  )
where

import Reflex
import Reflex.Host.Basic
import Reflex.Gloss.Event

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar, readTVarIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Graphics.Gloss (Color, Display, Picture, blank)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Exit (exitSuccess)

import qualified Graphics.Gloss.Interface.IO.Game as G

-- | Synonym for a Gloss Event to prevent name clashes.
type InputEvent = G.Event

-- | Convert the refresh and input events to a @'Dynamic' 'Picture'@
type GlossApp t m
  = BasicGuestConstraints t m
  => Event t Float
  -> EventSelector t GlossEvent
  -> BasicGuest t m (Dynamic t Picture, Event t ())

-- | Play the 'GlossApp' in a window
playReflex
  :: Display -- ^ Display mode.
  -> Color -- ^ Background color.
  -> Int -- ^ Maximum frames per second.
  -> (forall t m. GlossApp t m) -- ^ A reflex function that returns a 'Behavior t Picture'
  -> IO ()
playReflex display color frequency network =
  basicHostWithQuit $ do
    picTVar <- liftIO $ newTVarIO blank
    quitTVar <- liftIO $ newTVarIO False

    (tickEvent, tickTrigger)  <- newTriggerEvent
    (inputEvent, inputTrigger) <- newTriggerEvent
    (hostQuitEvent, hostQuitTrigger) <- newTriggerEvent

    (dPicture, eQuit) <- network tickEvent (fan $ glossEventMap <$> inputEvent)

    performEvent_ $
      liftIO . atomically . writeTVar picTVar <$>
      updated dPicture

    performEvent_ $
      (liftIO . atomically $ writeTVar quitTVar True) <$ eQuit

    void . liftIO .
      flip forkFinally (\_ -> hostQuitTrigger ()) $
      playIO
        display
        color
        frequency
        ()
        (\_ -> readTVarIO picTVar)
        (\ge _ -> inputTrigger ge)
        (\fl _ -> do
            shouldQuit <- readTVarIO quitTVar
            if shouldQuit
              then exitSuccess
              else tickTrigger fl)

    pure ((), hostQuitEvent)
