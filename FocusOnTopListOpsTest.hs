-- Copyright 2023 Google LLC
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

import Test.HUnit

import FocusOnTopListOps

main = do
    let
        matchTuplePredicate :: (String, Integer) -> String -> Bool
        matchTuplePredicate (s, i) ss = s == ss

        emptyStringIntTupleList :: [(String, Integer)]
        emptyStringIntTupleList = []

        emptyIntList :: [Integer]
        emptyIntList = []

    runTestTT $ test [
        "remove" ~: [
            "single item in list and it matches predicate" ~: remove (\x y -> x == y)
                                                                     ["hi"] "hi" ~=? ([], Just "hi")

          , "longer list including an element that matches predicate" ~:
                remove (\x y -> x == y) ["asdf", "hi", "bye", "1234"] "hi" ~=?
                    (["asdf", "bye", "1234"], Just "hi")

          , "longer list including an element that matches predicate - also list type is different \
            \than match type" ~: remove matchTuplePredicate
                                        [("asdf", 1), ("hi", 2), ("bye", 3), ("1234", 4)] "hi" ~=?
                                            ([("asdf", 1), ("bye", 3), ("1234", 4)], Just ("hi", 2))

          , "single item in list and it does not matches predicate" ~: remove (\x y -> x == y)
                                                                              ["hi"] "bye" ~=?
                                                                                  (["hi"], Nothing)

          , "empty list" ~: remove (\x y -> x == y) [] "bye" ~=? ([], Nothing)
          ]
      , "removeAssoc" ~: [
            "single item in list and it matches search key" ~:
                removeAssoc [("asdf", 1)] "asdf" ~=? ([], Just ("asdf", 1))

          , "multiple items in list and one of them matches search key" ~:
                removeAssoc [("asdf", 1), ("1234", 2), ("hello", 3)] "1234" ~=?
                    ([("asdf", 1), ("hello", 3)], Just ("1234", 2))

          , "multiple items in list and none of them matches search key" ~:
                removeAssoc [("asdf", 1), ("1234", 2), ("hello", 3)] "1235" ~=?
                    ([("asdf", 1), ("1234", 2), ("hello", 3)], Nothing)

          , "empty list" ~: removeAssoc emptyStringIntTupleList "1235" ~=?
                (emptyStringIntTupleList, Nothing)
          ]
      , "reverseConcat" ~: [
            "normal case" ~: reverseConcat [1, 2, 3] [4, 5] ~=? [3, 2, 1, 4, 5]
          , "first list is empty" ~: reverseConcat [] [4, 5] ~=? [4, 5]
          , "second list is empty" ~: reverseConcat [1, 2, 3] [] ~=? [3, 2, 1]
          , "both lists are empty" ~: reverseConcat emptyIntList emptyIntList ~=? emptyIntList
          ]
      , "maybeCons" ~: [
            "regular example" ~: maybeCons ([1], Just 2) ~=? [2, 1]
          , "regular example with longer list" ~: maybeCons ([1, 2, 3], Just 5) ~=? [5, 1, 2, 3]
          , "Nothing to cons" ~: maybeCons ([1, 2, 3], Nothing) ~=? [1, 2, 3]
          , "Nothing to cons and empty list" ~: maybeCons (emptyIntList, Nothing) ~=? emptyIntList
          , "empty list" ~: maybeCons ([], Just 9) ~=? [9]
          ]
      , "reorder" ~: [
            "single element and it matches the ordering" ~: reorder [("asdf", 1)] ["asdf"] ~=?
                [("asdf", 1)]

          , "multiple elements and they all match the ordering" ~:
                reorder [("asdf", 1), ("1234", 2), ("goodbye", 3)] ["asdf", "1234", "goodbye"] ~=?
                    [("asdf", 1), ("1234", 2), ("goodbye", 3)]

          , "multiple elements and they all match elements in the ordering \
            \but need to be shuffled" ~: reorder [("asdf", 1), ("1234", 2), ("goodbye", 3)]
                                                 ["1234", "goodbye", "asdf"] ~=?
                                                     [("1234", 2), ("goodbye", 3), ("asdf", 1)]

          , "multiple elements and they all match elements in the ordering but need to be shuffled.\
            \ Order list has extra values." ~: reorder [("asdf", 1), ("1234", 2), ("goodbye", 3)]
                                                       ["1", "1234", "d", "goodbye", "p",
                                                        "j", "asdf", "f"] ~=?
                                                            [("1234", 2), ("goodbye", 3),
                                                             ("asdf", 1)]

          , "multiple elements, only some of which match elements in the ordering but need to be \
            \shuffled" ~: reorder [("s", 3), ("asdf", 1), ("", 5), ("1234", 2),
                                   ("o", 5), ("j", 8), ("goodbye", 3), ("k", 8)]
                                  ["1234", "goodbye", "asdf"] ~=?
                                      [("s", 3), ("", 5), ("o", 5), ("j", 8),
                                       ("k", 8), ("1234", 2), ("goodbye", 3), ("asdf", 1)]

          , "multiple elements, but empty ordering list" ~:
                reorder [("asdf", 1), ("1234", 2), ("goodbye", 3)] [] ~=?
                    [("asdf", 1), ("1234", 2), ("goodbye", 3)]

          , "empty list, non-empty ordering" ~: reorder emptyStringIntTupleList
                                                        ["asdf", "1234", "goodbye"] ~=?
                                                            emptyStringIntTupleList

          , "both lists empty" ~: reorder emptyStringIntTupleList [] ~=? emptyStringIntTupleList
          ]
      ]
