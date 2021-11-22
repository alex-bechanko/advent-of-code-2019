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
module Advent.Util
    ( Solution
    , Parser
    , runSolution
    , maybeToEither
    , safeHead
    ) where

type Solution err a = a -> Either err String
type Parser err a = String -> Either err a

runSolution :: Show err => Parser err a -> Solution err a -> Solution err a -> String -> Either String (String, String)
runSolution parse soln1 soln2 inp = case runSolution' parse soln1 soln2 inp of
    Left x -> Left (show x)
    Right y -> Right y

runSolution' parse soln1 soln2 inp = do
    inp <- parse inp
    s1 <- soln1 inp
    s2 <- soln2 inp
    return (s1, s2)

maybeToEither e (Just x) = Right x
maybeToEither e Nothing = Left e

safeHead :: a -> [b] -> Either a b
safeHead e [] = Left e
safeHead e (x:xs) = Right x