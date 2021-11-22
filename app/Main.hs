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
module Main where
import System.Console.ArgParser
import System.IO (readFile)
import Control.Applicative ((<$>))
import Control.Exception (try, IOException)
import Control.Monad (join)

import qualified Advent.Day01 as Day01
import qualified Advent.Day02 as Day02

import Data.List (elemIndex)

data CLI = CLI Int String
    deriving (Show)

data Error = InvalidDay Int | InvalidPath String | SolutionError String
    deriving (Eq)

instance Show Error where
    show (InvalidDay x) = "Invalid day '" ++ show x ++ "' found. Please check that the day is valid number between 1-25."
    show (InvalidPath x) = "Error reading data from file '" ++ x ++ "'. Please check that the file is readable and exists."
    show (SolutionError x) = "Error occurred computing solution: " ++ x


mapErr (Left x) = Left (SolutionError x)
mapErr (Right x) = Right x

parser = CLI
    `parsedBy` reqPos "day"   `Descr` "The day for to compute solutions for"
    `andBy`    reqPos "input" `Descr` "The path to the data file used by the solutions"

interface =
    (`setAppDescr` "Compute solutions to Advent of Code 2019")
    <$> mkApp parser

main :: IO ()
main = do
    cli <- interface
    runApp cli main'

solutionData :: String -> IO (Either Error String)
solutionData path = do 
    inp <- try (readFile path) :: IO (Either IOException String)
    case inp of
        Left e -> return (Left (InvalidPath path))
        Right contents ->  return (Right contents)

solutions :: [String -> Either Error (String, String)]
solutions = [mapErr . Day01.solution, mapErr . Day02.solution]

solution :: Int -> Either Error (String -> Either Error (String, String))
solution n 
    | n > length solutions = Left (InvalidDay n) 
    | n - 1 < 0                = Left (InvalidDay n)
    | otherwise                = Right  (solutions !! (n - 1))

main' (CLI day path) = do
    solnData <- solutionData path
    let ans = (solnData >>=) =<< solution day
        
    case ans of 
        Right (s1, s2) -> do
            putStrLn $ "Solution 1: " ++ s1
            putStrLn $ "Solution 2: " ++ s2
        Left err -> print err