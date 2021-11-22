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
module Advent.Day02Test (tests) where
import Advent.Day02

import Test.Tasty
import Test.Tasty.HUnit
import Data.Sequence



tests = testGroup "Day02"
    [ test_parse
    , test_readMem
    , test_readPtr
    , test_setMem
    , test_addInstr
    , test_mulInstr
    , test_runProgram
    ]

test_parse = testGroup "parse" 
    [ testCase "parse \"1,2,3,4\" = Right Seq [1,2,3,4]"  $ parse "1,2,3,4"    @?= Right (fromList [1,2,3,4])
    , testCase "parse \"1,2,3,4asd\" = Left ParseFailure" $ parse "1,2,3,4asd" @?= Left ParseFailure
    ]

test_readMem = testGroup "readMem"
    [ testCase "readMem (Seq [1,2,3,4]) 0 = Right 1"               $ readMem (fromList [1,2,3,4]) 0 @?= Right 1
    , testCase "readMem (Seq [1,2,3,4]) 1 = Right 2"               $ readMem (fromList [1,2,3,4]) 1 @?= Right 2
    , testCase "readMem (Seq [1,2,3,4]) 2 = Right 3"               $ readMem (fromList [1,2,3,4]) 2 @?= Right 3
    , testCase "readMem (Seq [1,2,3,4]) 3 = Right 4"               $ readMem (fromList [1,2,3,4]) 3 @?= Right 4
    , testCase "readMem (Seq [1,2,3,4]) 4 = Left InvalidAddress 4" $ readMem (fromList [1,2,3,4]) 4 @?= Left (InvalidAddress 4)
    ]

test_readPtr = testGroup "readPtr"
    [ testCase "readPtr (Seq [1,2,3,4]) 0 = Right 2"               $ readPtr (fromList [1,2,3,4]) 0 @?= Right 2
    , testCase "readPtr (Seq [1,2,3,4]) 1 = Right 3"               $ readPtr (fromList [1,2,3,4]) 1 @?= Right 3
    , testCase "readPtr (Seq [1,2,3,4]) 2 = Right 4"               $ readPtr (fromList [1,2,3,4]) 2 @?= Right 4
    , testCase "readPtr (Seq [1,2,3,4]) 3 = Left InvalidAddress 4" $ readPtr (fromList [1,2,3,4]) 3 @?= Left (InvalidAddress 4)
    ]

test_setMem = testGroup "setMem"
    [ testCase "setMem (Seq [1,2,3,4]) 0 99 = Right Seq [99,2,3,4]"    $ setMem (fromList [1,2,3,4]) 0 99 @?= Right (fromList [99,2,3,4])
    , testCase "setMem (Seq [1,2,3,4]) 1 99 = Right Seq [1,99,3,4]"    $ setMem (fromList [1,2,3,4]) 1 99 @?= Right (fromList [1,99,3,4])
    , testCase "setMem (Seq [1,2,3,4]) 4 99 = Left (InvalidAddress 4)" $ setMem (fromList [1,2,3,4]) 4 99 @?= Left (InvalidAddress 4)
    ]

test_addInstr = testGroup "addInstr" 
    [ testCase "addInstr (Seq [1,2,3,4,5]) 0 = Right Seq [1,2,3,4,7]" $ addInstr (fromList [1,2,3,4,5]) 0 @?= Right (fromList [1,2,3,4,7])
    , testCase "addInstr (Seq [1,0,0,0]) 0 = Right Seq [2,0,0,0]"     $ addInstr (fromList [1,0,0,0]) 0   @?= Right (fromList [2,0,0,0])
    ]

test_mulInstr = testGroup "mulInstr"
    [ testCase "mulInstr (Seq [1,2,3,4,5]) 0 = Right Seq [1,2,3,4,12]" $ mulInstr (fromList [1,2,3,4,5]) 0 @?= Right (fromList [1,2,3,4,12])
    , testCase "mulInstr (Seq [1,2,4,0,5]) 0 = Right Seq [20,2,4,0,5]" $ mulInstr (fromList [1,2,4,0,5]) 0 @?= Right (fromList [20,2,4,0,5])
    ]

test_runProgram = testGroup "runProgram"
    [ testCase "runProgram (Seq [1,0,0,0,99]) = Right Seq [2,0,0,0,99]" $ runProgram (fromList [1,0,0,0,99]) @?= Right (fromList [2,0,0,0,99])
    ]