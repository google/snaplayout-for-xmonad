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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

---------------------------------------------------------------------------------------------------
-- |
-- Module      :  SnapLayout
-- Description :  Layout based on snapping windows to particular edges of the screen.
-- Copyright   :  (c) Google LLC
-- License     :  Apache-2.0 (see LICENSE)
--
-- Maintainer  :  hartmanng@google.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- LayoutClass that lets the user snap windows to particular edges of the screen, at 1/2 screen
-- width or height.
---------------------------------------------------------------------------------------------------

module SnapLayout (
    -- * Usage
    -- $usage
    SnapLayout(..),
    SnapLoc(..),
    Snap(..)
  ) where

import XMonad
import Data.Default
import Control.Monad

import qualified XMonad.StackSet as W
import qualified Data.Map as Map

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import SnapLayout
--
-- Then edit your @layoutHook@ by adding the SnapLayout:
--
-- > myLayout = SnapLayout def ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- > , ((modm .|. controlMask, xK_KP_Left ), withFocused (sendMessage . Snap SnapLayout.Left))
-- > , ((modm .|. controlMask, xK_KP_Right), withFocused (sendMessage . Snap SnapLayout.Right))
-- > , ((modm .|. controlMask, xK_KP_Up   ), withFocused (sendMessage . Snap Top))
-- > , ((modm .|. controlMask, xK_KP_Down ), withFocused (sendMessage . Snap Bottom))
-- > ...
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- SnaLoc represents a "location" that a window can be snapped to.
data SnapLoc = Top | Bottom | Left | Right deriving (Show, Read)

-- Snap is a message that can be sent to SnapLayout in response to user input, which instructs the
-- layout to snap a window to a particular SnapLoc.
data Snap = Snap SnapLoc Window
instance Message Snap

-- SnapLayout is a LayoutClass that lets the user snap windows to particular edges of the screen,
-- at 1/2 screen width or height.
data SnapLayout a = SnapLayout { snapped :: !(Map.Map Window SnapLoc)
                               }
                               deriving (Show, Read)

instance Default (SnapLayout a) where
    def = SnapLayout Map.empty

instance LayoutClass SnapLayout Window where
    -- pureLayout is responsible for the actual positioning of windows on the screen.
    pureLayout (SnapLayout mp) r s = map layout (W.integrate s)
        where
              -- layout compuates the location and bounds of a single window, and returns a tuple of
              -- the bounds with the window itself.
              layout w = (w, computeRect (Map.lookup w mp))

              -- computeRect compuates the location and bounds of a single window.
              computeRect Nothing = r
              computeRect (Just sl) = rectForLoc r sl

              -- rectForLoc translates a parent Rectangle and a SnapLoc into a child Rectangle to
              -- render the window in.
              rectForLoc (Rectangle x y w h) Top = Rectangle x y w (h `div` 2)
              rectForLoc (Rectangle x y w h) Bottom =
                  Rectangle x (y + fromIntegral (h `div` 2)) w (h `div` 2)
              rectForLoc (Rectangle x y w h) SnapLayout.Left = Rectangle x y (w `div` 2) h
              rectForLoc (Rectangle x y w h) SnapLayout.Right =
                  Rectangle (x + fromIntegral (w `div` 2)) y (w `div` 2) h

    -- pureMessage receives messages from user actions.
    pureMessage (SnapLayout mp) m = msum [fmap snap (fromMessage m)]
        where snap (Snap l w) = SnapLayout (Map.insert w l mp)

    -- description does something, probably.
    description _ = "SnapLayout"
