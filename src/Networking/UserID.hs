module Networking.UserID where

import Data.Char
import System.Random

mapToChar :: Int -> Char
mapToChar val
    | 0 <= val && val <= 9 = chr (val + 48)
    | 10 <= val && val <= 35 = chr (val + 55)
    | 36 <= val && val <= 61 = chr (val + 61)
    | otherwise = '-'

-- This is "probably" unique
newRandomUserID :: IO String
newRandomUserID = map mapToChar . take 128 . randomRs (0, 61) <$> getStdGen
