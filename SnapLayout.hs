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

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, InstanceSigs #-}

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
-- LayoutClass that lets the user snap windows to particular edges or corners of the screen, at
-- various fractions of screen width and/or height.
---------------------------------------------------------------------------------------------------

module SnapLayout (
    -- * Usage
    -- $usage
    SnapLayout(..),
    SnapLoc(..),
    Snap(..),
    FineAdjustmentMessage(..),
    FineAdjustmentDirection(..),
    Unsnap(..),
    Unadjust(..)
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
-- > myLayout = snapLayout ||| Full ||| etc..
-- >   where
-- >     snapLayout :: SnapLayout a
-- >     snapLayout = def
-- > main = xmonad def { layoutHook = myLayout }
--
-- Or, to customize the fine adjustment increment:
--
-- > myLayout = snapLayout ||| Full ||| etc..
-- >   where
-- >     snapLayout :: SnapLayout a
-- >     snapLayout = def { adjustmentDelta = 3/100 }
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_the_layout_hook
--
-- In the key-bindings, do something like:
--
-- > , ((modm .|. controlMask, xK_KP_End      ), withFocused (sendMessage . Snap BottomLeft))
-- > , ((modm .|. controlMask, xK_KP_Down     ), withFocused (sendMessage . Snap Bottom))
-- > , ((modm .|. controlMask, xK_KP_Page_Down), withFocused (sendMessage . Snap BottomRight))
-- > , ((modm .|. controlMask, xK_KP_Left     ), withFocused (sendMessage . Snap SnapLayout.Left))
-- > , ((modm .|. controlMask, xK_KP_Right    ), withFocused (sendMessage . Snap SnapLayout.Right))
-- > , ((modm .|. controlMask, xK_KP_Home     ), withFocused (sendMessage . Snap TopLeft))
-- > , ((modm .|. controlMask, xK_KP_Up       ), withFocused (sendMessage . Snap Top))
-- > , ((modm .|. controlMask, xK_KP_Page_Up  ), withFocused (sendMessage . Snap TopRight))
-- > , ((modm .|. controlMask, xK_KP_Insert   ), withFocused (sendMessage . Unsnap))
-- >
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_End),
-- >       withFocused (sendMessage . FineAdjustmentMessage BottomLeftAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Down),
-- >       withFocused (sendMessage . FineAdjustmentMessage BottomAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Page_Down),
-- >       withFocused (sendMessage . FineAdjustmentMessage BottomRightAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Left),
-- >       withFocused (sendMessage . FineAdjustmentMessage LeftAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Right),
-- >       withFocused (sendMessage . FineAdjustmentMessage RightAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Home),
-- >       withFocused (sendMessage . FineAdjustmentMessage TopLeftAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Up),
-- >       withFocused (sendMessage . FineAdjustmentMessage TopAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Page_Up),
-- >       withFocused (sendMessage . FineAdjustmentMessage TopRightAdjustment))
-- > , ((modm .|. controlMask .|. shiftMask, xK_KP_Insert), withFocused (sendMessage . Unadjust))
-- > ...
--
-- For detailed instruction on editing the key binding see:
--
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_key_bindings

-- SnapLoc represents a "location" that a window can be snapped to.
data SnapLoc = Top | Bottom | Left | Right | TopLeft | TopRight | BottomLeft | BottomRight
    deriving (Show, Read, Eq)

-- rectForLoc translates a parent Rectangle and a SnapLoc into a child Rectangle to render the
-- window in.
rectForLoc :: Rectangle -> SnapLoc -> Rational -> Rectangle
rectForLoc r Top              s = topPart r s
rectForLoc r Bottom           s = bottomPart r s
rectForLoc r SnapLayout.Left  s = leftPart r s
rectForLoc r SnapLayout.Right s = rightPart r s
rectForLoc r TopLeft          s = topPart (leftPart r s) s
rectForLoc r TopRight         s = topPart (rightPart r s) s
rectForLoc r BottomLeft       s = bottomPart (leftPart r s) s
rectForLoc r BottomRight      s = bottomPart (rightPart r s) s

-- topPart returns the top portion of a Rectangle, as specified by the passed-in Rational.
topPart :: Rectangle -> Rational -> Rectangle
topPart (Rectangle x y w h) s = Rectangle x y w (dimensionTimesRational h s)

-- bottomPart returns the bottom portion of a Rectangle, as specified by the passed-in Rational.
bottomPart :: Rectangle -> Rational -> Rectangle
bottomPart (Rectangle x y w h) s =
    Rectangle x (latterPartPosition y h s) w (dimensionTimesRational h s)

-- leftPart returns the left portion of a Rectangle, as specified by the passed-in Rational.
leftPart :: Rectangle -> Rational -> Rectangle
leftPart (Rectangle x y w h) s = Rectangle x y (dimensionTimesRational w s) h

-- rightPart returns the right portion of a Rectangle, as specified by the passed-in Rational.
rightPart :: Rectangle -> Rational -> Rectangle
rightPart (Rectangle x y w h) s =
    Rectangle (latterPartPosition x w s) y (dimensionTimesRational w s) h

-- latterPartPosition computes the top-left corner Position of the right or bottom portions of a
-- Rectangle.
latterPartPosition :: Position -> Dimension -> Rational -> Position
latterPartPosition p d r = p + fromIntegral (dimensionTimesRational d (1 - r))

-- dimensionTimesRational multiplies a Dimension by a Rational and converts it back to a (floor'd)
-- Dimension.
dimensionTimesRational :: Dimension -> Rational -> Dimension
dimensionTimesRational d r = floor (fromIntegral d * r)

-- adjustSnappedRect resizes a snapped rect given width and height adjustments.
adjustSnappedRect :: Rectangle -> SnapLoc -> Integer -> Integer -> Rectangle
adjustSnappedRect r Top              wd hd = adjustTop r hd
adjustSnappedRect r Bottom           wd hd = adjustBottom r hd
adjustSnappedRect r SnapLayout.Left  wd hd = adjustLeft r wd
adjustSnappedRect r SnapLayout.Right wd hd = adjustRight r wd
adjustSnappedRect r TopLeft          wd hd = adjustTop (adjustLeft r wd) hd
adjustSnappedRect r TopRight         wd hd = adjustTop (adjustRight r wd) hd
adjustSnappedRect r BottomLeft       wd hd = adjustBottom (adjustLeft r wd) hd
adjustSnappedRect r BottomRight      wd hd = adjustBottom (adjustRight r wd) hd

-- adjustTop "adjusts" a top-snapped Rectangle by moving the bottom edge.
adjustTop :: Rectangle -> Integer -> Rectangle
adjustTop (Rectangle x y w h) hd = Rectangle x y w (h + fromIntegral hd)

-- adjustBottom "adjusts" a bottom-snapped Rectangle by moving the top edge.
adjustBottom :: Rectangle -> Integer -> Rectangle
adjustBottom (Rectangle x y w h) hd = Rectangle x (y + fromIntegral hd) w (h - fromIntegral hd)

-- adjustLeft "adjusts" a left-snapped Rectangle by moving the right edge.
adjustLeft :: Rectangle -> Integer -> Rectangle
adjustLeft (Rectangle x y w h) wd = Rectangle x y (w + fromIntegral wd) h

-- adjustRight "adjusts" a right-snapped Rectangle by moving the left edge.
adjustRight :: Rectangle -> Integer -> Rectangle
adjustRight (Rectangle x y w h) wd = Rectangle (x + fromIntegral wd) y (w - fromIntegral wd) h

-- FullLoc describes the complete location of a window, including the SnapLoc to describe where the
-- window is snapped to, and the adjusted window size.
data FullLoc = FullLoc { snapLoc :: SnapLoc     -- ^ Location of the screen to which the window is snapped (default: SnapLayout.Left)
                       , baseSize :: Rational   -- ^ Initial proportion of the screen a snapped window takes up, before being adjusted (default: 1/2)
                       , widthDelta :: Integer  -- ^ Change in wihdt (in some increment defined elsewhere) from the baseSize (default: 0)
                       , heightDelta :: Integer -- ^ Change in height (in some increment defined elsewhere) from the baseSize (default: 0)
                       }
                       deriving (Show, Read)

instance Default FullLoc where
    def = FullLoc SnapLayout.Left (1 / 2) 0 0

-- cycleSnapModes either:
--   a) snaps a window to the given SnapLoc at the default baseSize if it was not already snapped,
--      or was snapped to a different SnapLoc, OR
--   b) cycles between baseSizes (1/2 -> 1/3 -> 2/3 -> 1/2 ...)
cycleSnapModes :: SnapLoc -> Maybe FullLoc -> FullLoc
cycleSnapModes l Nothing = adfl l
cycleSnapModes l (Just (FullLoc sl bs _ _))
    | l == sl   = cycleSnapModes' sl bs
    | otherwise = adfl l

-- cycleSnapModes' is a helper for cycleSnapModes. Unburdened by the default cases in
-- cycleSnapModes, all we have to do here is cycle between 1/2-screen, 1/3-screen, and
-- 2/3-screen snap modes.
cycleSnapModes' :: SnapLoc -> Rational -> FullLoc
cycleSnapModes' l r
    | r == (1 / 2) = def {snapLoc = l, baseSize = 1 / 3}
    | r == (1 / 3) = def {snapLoc = l, baseSize = 2 / 3}
    | otherwise    = adfl l

-- adfl is the "almost-default" FullLoc, with a given SnapLoc. I'm great at naming.
adfl :: SnapLoc -> FullLoc
adfl l = def {snapLoc = l}

-- Snap is a message that can be sent to SnapLayout in response to user input, which instructs the
-- layout to snap a window to a particular SnapLoc.
data Snap = Snap SnapLoc Window
instance Message Snap

-- FineAdjustmentDirection is the direction to resize a window.
data FineAdjustmentDirection = TopAdjustment | BottomAdjustment | LeftAdjustment | RightAdjustment |
    TopLeftAdjustment | TopRightAdjustment | BottomLeftAdjustment | BottomRightAdjustment

-- adjustWidth adds to a counter of width adjustments by FineAdjustmentDirection.
adjustWidth :: Integer -> FineAdjustmentDirection -> Integer
adjustWidth i TopAdjustment         = i
adjustWidth i BottomAdjustment      = i
adjustWidth i LeftAdjustment        = i - 1
adjustWidth i RightAdjustment       = i + 1
adjustWidth i TopLeftAdjustment     = i - 1
adjustWidth i TopRightAdjustment    = i + 1
adjustWidth i BottomLeftAdjustment  = i - 1
adjustWidth i BottomRightAdjustment = i + 1

-- adjustHeight adds to a counter of height adjustments by FineAdjustmentDirection.
adjustHeight :: Integer -> FineAdjustmentDirection -> Integer
adjustHeight i TopAdjustment         = i - 1
adjustHeight i BottomAdjustment      = i + 1
adjustHeight i LeftAdjustment        = i
adjustHeight i RightAdjustment       = i
adjustHeight i TopLeftAdjustment     = i - 1
adjustHeight i TopRightAdjustment    = i - 1
adjustHeight i BottomLeftAdjustment  = i + 1
adjustHeight i BottomRightAdjustment = i + 1

-- FineAdjustmentMessage is a message that can be sent to SnapLayout in response to user input,
-- which instructs the layout to resize a snapped window in a particular direction.
data FineAdjustmentMessage = FineAdjustmentMessage FineAdjustmentDirection Window
instance Message FineAdjustmentMessage

-- Unsnap is a message that can be sent to SnapLayout in response to user input, which instructs the
-- layout to unsnap a window.
data Unsnap = Unsnap Window
instance Message Unsnap

-- Unadjust is a message that can be sent to SnapLayout in response to user input, which instructs
-- the layout to reset fine adjustments for a window.
data Unadjust = Unadjust Window
instance Message Unadjust

-- SnapLayout is a LayoutClass that lets the user snap windows to particular edges or corners of the
-- screen, at various fractions of screen width and/or height.
data SnapLayout a = SnapLayout { snapped :: !(Map.Map Window FullLoc) -- ^ Map used internally to keep track of Window states (default: Map.empty)
                               , adjustmentDelta :: Rational          -- ^ Percent of screen to adjust by in response to each FineAdjustmentMessage (default: 3/100)
                               }
                               deriving (Show, Read)

instance Default (SnapLayout a) where
    def = SnapLayout Map.empty (3 / 100)

instance LayoutClass SnapLayout Window where
    -- pureLayout is responsible for the actual positioning of windows on the screen.
    pureLayout :: SnapLayout Window -> Rectangle -> W.Stack Window -> [(Window, Rectangle)]
    pureLayout (SnapLayout mp ad) (Rectangle x y w h) (W.Stack f u d) = map layout (f : u ++ d)
        where
              -- layout compuates the location and bounds of a single window, and returns a tuple of
              -- the bounds with the window itself.
              layout :: Window -> (Window, Rectangle)
              layout w = (w, computeRect (Map.lookup w mp))

              -- computeRect compuates the location and bounds of a single window.
              computeRect :: Maybe FullLoc -> Rectangle
              computeRect Nothing = r
              computeRect (Just (FullLoc sl bs wd hd)) =
                  adjustSnappedRect (rectForLoc r sl bs) sl
                                    (floor $ ad * fromIntegral wd * fromIntegral w)
                                    (floor $ ad * fromIntegral hd * fromIntegral h)

              -- r is the original Rectangle that was passed into `pureLayout`.
              r :: Rectangle
              r = Rectangle x y w h

    -- pureMessage receives messages from user actions.
    pureMessage :: SnapLayout Window -> SomeMessage -> Maybe (SnapLayout Window)
    pureMessage (SnapLayout mp ad) m = msum [ fmap snap (fromMessage m)
                                            , fmap adjust (fromMessage m)
                                            , fmap unsnap (fromMessage m)
                                            , fmap unadjust (fromMessage m)
                                            ]
        where
              -- snap instructs the layout to snap a window to a location.
              snap :: Snap -> SnapLayout Window
              snap (Snap l w) = SnapLayout (Map.insert w (cycleSnapModes l (Map.lookup w mp)) mp) ad

              -- adjust instructs the layout to resize a snapped window.
              adjust :: FineAdjustmentMessage -> SnapLayout Window
              adjust (FineAdjustmentMessage d w) = adjustRect d w (Map.lookup w mp)

              -- adjustRect is a helper for `adjust` to unpack the `Maybe`.
              adjustRect :: FineAdjustmentDirection -> Window -> Maybe FullLoc -> SnapLayout Window
              adjustRect d w Nothing = sl
              adjustRect d w (Just (FullLoc sl bs wd hd)) =
                  SnapLayout (Map.insert w (FullLoc sl bs (adjustWidth wd d)
                                                          (adjustHeight hd d)) mp) ad

              -- unsnap instructs the layout to unsnap a window.
              unsnap :: Unsnap -> SnapLayout Window
              unsnap (Unsnap w) = SnapLayout (Map.delete w mp) ad

              -- unadjust instructs the layout to reset a window's find adjustments.
              unadjust :: Unadjust -> SnapLayout Window
              unadjust (Unadjust w) = unadjustRect w (Map.lookup w mp)

              -- unadjustRect is a helper for `unadjust` to unpack the `Maybe`.
              unadjustRect :: Window -> Maybe FullLoc -> SnapLayout Window
              unadjustRect w Nothing = sl
              unadjustRect w (Just (FullLoc sl bs _ _)) =
                  SnapLayout (Map.insert w (FullLoc sl bs 0 0) mp) ad

              -- sl is the original SnapLayout that was passed into `pureMessage`.
              sl :: SnapLayout Window
              sl = SnapLayout mp ad

    -- description does something, probably.
    description :: SnapLayout Window -> String
    description (SnapLayout mp ad) = "SnapLayout"
