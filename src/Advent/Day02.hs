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
module Advent.Day02
    ( solution
    , parse
    , readMem
    , readPtr
    , setMem
    , addInstr
    , mulInstr
    , runProgram
    , solutionGeneral
    , Error(..)
    ) where
import Advent.Util hiding (Error(..))

import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Sequence (Seq, fromList, update, lookup)


data Error = ParseFailure | InvalidAddress Int | NoSolution
    deriving (Eq)

instance Show Error where 
    show ParseFailure = "Error occurred while parsing the data in the file"
    show (InvalidAddress addr) = "Attempted to use out of bounds address '" ++ show addr ++ "'"
    show NoSolution = "Unable to find a solution"

setMem mem ip val 
    | ip >= length mem = Left $ InvalidAddress ip
    | otherwise = Right $ update ip val mem

readMem mem ip = maybeToEither (InvalidAddress ip) $ lookup ip mem

readPtr mem ip = do
    addr <- readMem mem ip
    readMem mem addr

addInstr :: Seq Int -> Int -> Either Error (Seq Int)
addInstr mem ip = do
    a <- readPtr mem (ip + 1)
    b <- readPtr mem (ip + 2)
    c <- readMem mem (ip + 3)

    setMem mem c (a + b)

mulInstr :: Seq Int -> Int -> Either Error (Seq Int)
mulInstr mem ip = do
    a <- readPtr mem (ip + 1)
    b <- readPtr mem (ip + 2)
    c <- readMem mem (ip + 3)

    setMem mem c (a * b)
    

fromInstr x = case x of
    1  -> Right addInstr
    2  -> Right mulInstr
    _  -> Left $ InvalidAddress x

runProgram :: Seq Int -> Either Error (Seq Int)
runProgram mem = runProgram' mem 0

runProgram' mem ip = do
    instr <- readMem mem ip
    case instr of
        99 -> return mem
        _  -> do
            op    <- fromInstr instr
            mem'  <- op mem ip
            runProgram' mem' (ip + 4)


solutionGeneral solnData x y = do
    mem <- setMem' 1 x solnData >>= setMem' 2 y >>= runProgram
    readMem mem 0
  where
    setMem' x y mem = setMem mem x y

parse :: String -> Either Error (Seq Int)
parse d = maybeToEither ParseFailure . mapM readMaybe . fromList $ splitOn "," d

solution1 solnData = fmap show $ solutionGeneral solnData 12 2

solution2 :: Seq Int -> Either Error String
solution2 solnData = safeHead NoSolution . map (show . answer) . filter isAnswer $ [(x,y) | x <- [1..99], y <- [1..99]]
    where
        isAnswer (x,y) = (== Right True) . fmap (==19690720) $ solutionGeneral solnData x y 
        answer (x,y) = x * 100 + y
        
solution :: String -> Either String (String, String)
solution = runSolution parse solution1 solution2