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
import Test.Tasty
import Test.Tasty.HUnit

import Advent.Day01
import qualified Advent.Day01Test as Day01Test
import qualified Advent.Day02Test as Day02Test

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
    [ Day01Test.tests
    , Day02Test.tests
    ]