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
module Advent.Day01
    ( solution
    , simpleFuel
    , complexFuel
    , solution1
    , solution2
    , parse
    , Error(..)
    ) where
import Text.Read (readMaybe)
import Advent.Util hiding (Error(..))

data Error = ParseFailure
    deriving (Eq)

instance Show Error where
    show ParseFailure = "Error occurred while parsing the data in the file"

parse :: String -> Either Error [Int]
parse puzzleData = maybeToEither ParseFailure . mapM readMaybe $ lines puzzleData

simpleFuel mass = mass `div` 3 - 2

complexFuel mass
    | fuel <= 0 = 0
    | otherwise = fuel + complexFuel fuel
  where
    fuel = simpleFuel mass

solution1 :: [Int] -> Either Error String
solution1 solnData = Right $ show . sum . map simpleFuel $ solnData

solution2 :: [Int] -> Either Error String 
solution2 solnData = Right $ show . sum . map complexFuel $ solnData

solution :: String -> Either String (String, String)
solution = runSolution parse solution1 solution2