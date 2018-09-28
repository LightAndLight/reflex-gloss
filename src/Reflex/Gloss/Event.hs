{-# language GADTs #-}
{-# language TemplateHaskell #-}
module Reflex.Gloss.Event
  ( GlossEvent(..)
  , Gloss.Key(..)
  , Gloss.SpecialKey(..)
  , Gloss.MouseButton(..)
  , Gloss.KeyState(..)
  , Gloss.Modifiers(..)
  , glossEventMap
  )
where

import Reflex
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.Functor.Identity (Identity)

import qualified Data.Dependent.Map as DMap
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

data GlossEvent a where
  GE_Key
    :: Maybe Gloss.Key
    -> Maybe Gloss.KeyState
    -> Maybe Gloss.Modifiers
    -> GlossEvent (Float, Float)
  GE_Motion :: GlossEvent (Float, Float)
  GE_Resize :: GlossEvent (Int, Int)

glossEventMap :: Gloss.Event -> DMap GlossEvent Identity
glossEventMap e =
  DMap.fromList $
  case e of
    Gloss.EventKey a b c d ->
      [ GE_Key (Just a) (Just b) (Just c) ==> d
      , GE_Key Nothing (Just b) (Just c) ==> d
      , GE_Key (Just a) Nothing (Just c) ==> d
      , GE_Key (Just a) (Just b) Nothing ==> d
      , GE_Key Nothing Nothing (Just c) ==> d
      , GE_Key Nothing (Just b) Nothing ==> d
      , GE_Key (Just a) Nothing Nothing ==> d
      , GE_Key Nothing Nothing Nothing ==> d
      ]
    Gloss.EventMotion a ->
      [GE_Motion ==> a]
    Gloss.EventResize a ->
      [GE_Resize ==> a]

deriveGEq ''GlossEvent
deriveGCompare ''GlossEvent
deriveGShow ''GlossEvent
