{-
Advent-of-Code 2019 solutions in Haskell
Copyright (C) 2021 Alexander Bechanko

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module Advent.Day01Test (tests) where
import Advent.Day01


import Test.Tasty
import Test.Tasty.HUnit


tests = testGroup "Day01"
    [ test_simpleFuel
    , test_complexFuel
    , test_parse
    , test_solution1
    , test_solution2
    , test_solution
    ]

test_simpleFuel = testGroup "simpleFuel"
    [ testCase "simpleFuel 12 = 2"         $ simpleFuel 12     @?= 2
    , testCase "simpleFuel 14 = 2"         $ simpleFuel 14     @?= 2
    , testCase "simpleFuel 1969 = 654"     $ simpleFuel 1969   @?= 654
    , testCase "simpleFuel 100756 = 33583" $ simpleFuel 100756 @?= 33583
    ]

test_complexFuel = testGroup "complexFuel" 
    [ testCase "complexFuel 12 = 2"         $ complexFuel 12     @?= 2
    , testCase "complexFuel 14 = 2"         $ complexFuel 14     @?= 2
    , testCase "complexFuel 1969 = 966"     $ complexFuel 1969   @?= 966
    , testCase "complexFuel 100756 = 50346" $ complexFuel 100756 @?= 50346
    ]

test_parse = testGroup "parse"
    [ testCase "parse \"1\\n2\\n3\" = Right [1,2,3]" $ parse "1\n2\n3" @?= Right [1,2,3]
    , testCase "parse \"1,2,3\" = Left ParseFailure" $ parse "1,2,3"   @?= Left ParseFailure
    ]

test_solution1 = testGroup "solution1"
    [ testCase "solution1 [12,14,1969] = Right \"658\"" $ solution1 [12, 14, 1969] @?= Right "658"
    ]

test_solution2 = testGroup "solution2"
    [ testCase "solution2 [12,14,1969] = Right \"970\"" $ solution2 [12, 14, 1969] @?= Right "970"
    ]

test_solution = testGroup "solution"
    [ testCase "solution \"12\\n14\\n1969\" = Right (\"658\", \"970\")" $ solution "12\n14\n1969" @?= Right ("658", "970")
    ]