-- Copyright 2022 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

---------------------------------------------------------------------------------------------------
-- |
-- Module      :  FocusOnTop
-- Description :  `LayoutModifier` that pulls the focused `Window` to the front of the screen.
-- Copyright   :  (c) Google LLC
-- License     :  Apache-2.0 (see LICENSE)
--
-- Maintainer  :  hartmanng@google.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- `LayoutModifier` that pulls the focused `Window` to the front of the screen, while preserving the
-- relative order of the rest of the `Window`s.
---------------------------------------------------------------------------------------------------

module FocusOnTop (
    -- * Usage
    -- $usage
    focusOnTop
  ) where

import XMonad
import XMonad.Layout.LayoutModifier

import qualified XMonad.StackSet as W

import FocusOnTopListOps

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import FocusOnTop
--
-- Then edit your @layoutHook@ by adding focusOnTop:
--
-- > myLayout = focusOnTop (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_the_layout_hook
--
-- This module works especially nicely with "SnapLayout":
--
-- > import SnapLayout
-- > import FocusOnTop
-- >
-- > myLayout = focusOnTop snapLayout ||| Full ||| etc..
-- >   where
-- >     snapLayout :: SnapLayout a
-- >     snapLayout = def
-- > main = xmonad def { layoutHook = myLayout }

type MaybeFocusOnTop = Maybe (FocusOnTop Window)
type MaybeStack      = Maybe (W.Stack Window)
type WinRect         = (Window, Rectangle)

-- | FocusOnTop is a `LayoutModifier` that pulls the focused `Window` to the front of the screen,
-- while preserving the relative order of the rest of the `Window`s.
data FocusOnTop a = FocusOnTop [a] deriving (Read, Show)

instance LayoutModifier FocusOnTop Window where
    -- | pureModifier reorders the `WinRect`s according to their previous order, to maintain
    -- consistency for the user, and then moves the focused `WinRect` to the front so that it
    -- appears on top of any other window.
    pureModifier :: FocusOnTop Window -> Rectangle -> MaybeStack -> [WinRect] ->
        ([WinRect], MaybeFocusOnTop)
    pureModifier (FocusOnTop o) _ ms wr = combine $ bringFocusToFront ms $ reorder wr o

-- | bringFocusToFront moves the focused window to the beginning of the list (and therefore to the
-- front of the screen).
bringFocusToFront :: MaybeStack -> [WinRect] -> [WinRect]
bringFocusToFront Nothing                wr = wr
bringFocusToFront (Just (W.Stack f _ _)) wr = maybeCons $ removeAssoc wr f

-- | combine bundles the `WinRect`s along with `FocusOnTop`'s new state to return to @XMonad@.
combine :: [WinRect] -> ([WinRect], MaybeFocusOnTop)
combine w = (w, Just (FocusOnTop $ windowsOnly w))

-- | windowsOnly extracts a list of `Window`s from the list of `WinRect`s
windowsOnly :: [WinRect] -> [Window]
windowsOnly w = map fst w

-- | focusOnTop applies the `FocusOnTop` `LayoutModifier` on the provided `Layout`, to pull the
-- focused `Window` to the front of the screen, while preserving the relative order of the rest of
-- the `Window`s.
focusOnTop :: l Window -> ModifiedLayout FocusOnTop l Window
focusOnTop = ModifiedLayout (FocusOnTop [])
