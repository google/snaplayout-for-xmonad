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

---------------------------------------------------------------------------------------------------
-- |
-- Module      :  FocusOnTopListOps
-- Description :  A collection of @List@ helper functions intended for "FocusOnTop".
-- Copyright   :  (c) Google LLC
-- License     :  Apache-2.0 (see LICENSE)
--
-- Maintainer  :  hartmanng@google.com
-- Stability   :  unstable
-- Portability :  portable
--
-- A small collection of helper @List@ operations that is primarily intended for use with the
-- "FocusOnTop" module. They are, however, generic, and therefore could be used
-- elsewhere, in theory.
---------------------------------------------------------------------------------------------------

module FocusOnTopListOps (
    remove
  , removeAssoc
  , reverseConcat
  , maybeCons
  , reorder
  ) where

-- | remove takes an equality predicate of @a@ and @b@, a list of @a@, and a @b@, and removes the
-- first occurrence of the @b@ from the list of @a@, and returns it (if applicable). This is similar
-- to @Data.List@'s
-- [deleteBy](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:deleteBy),
-- except that it also returns the deleted element, if any.
remove :: (a -> b -> Bool) -> [a] -> b -> ([a], Maybe a)
remove = remove' []

-- | remove' is a helper for `remove` with an accumulator.
remove' :: [a] -> (a -> b -> Bool) -> [a] -> b -> ([a], Maybe a)
remove' acc _  []     _ = (reverse acc, Nothing)
remove' acc eq (e:es) w
    | eq e w            = (reverseConcat acc es, Just e)
    | otherwise         = remove' (e:acc) eq es w

-- | removeAssoc is a specialization of `remove` for an association list.
removeAssoc :: Eq a => [(a, b)] -> a -> ([(a, b)], Maybe (a, b))
removeAssoc = remove (\x y -> fst x == y)

-- | reverseConcat reverses the first list and then concatenates it with the second.
reverseConcat :: [a] -> [a] -> [a]
reverseConcat es l = foldl (flip (:)) l es

-- | maybeCons will @cons@ the second element of the tuple to the first, if it exists. Otherwise it
-- returns the first element as-is.
maybeCons :: ([a], Maybe a) -> [a]
maybeCons (l, Nothing)  = l
maybeCons (l, Just e)   = e:l

-- | reorder takes an association list, @l@, and a list of keys to the association list, @o@. The
-- latter represents the desired ordering of the former. Any elements from @l@ that match keys in
-- @o@ will be shuffled to match the relative ordering of @o@, and any elements in @l@ that are not
-- found in @o@ will maintain their relative order. The matched elements will appear after the
-- unmatched elements in the result.
reorder :: Eq a => [(a, b)] -> [a] -> [(a, b)]
reorder = reorder' []

-- | reorder' is a helper for `reorder` with an accumulator.
reorder' :: Eq a => [(a, b)] -> [(a, b)] -> [a] -> [(a, b)]
reorder' acc l []     = l ++ reverse acc
reorder' acc l (o:os) = reorder'' acc os $ removeAssoc l o

-- | reorder'' is a helper for reorder' to unpack the tuple and @Maybe@ that result from the call to
-- `removeAssoc` and add the removed element (if it exists) to the accumulator.
reorder'' :: Eq a => [(a, b)] -> [a] -> ([(a, b)], Maybe (a, b)) -> [(a, b)]
reorder'' acc f (l, Nothing)  = reorder' acc l f
reorder'' acc f (l, Just e)   = reorder' (e:acc) l f
