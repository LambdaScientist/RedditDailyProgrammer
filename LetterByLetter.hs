module Reddit where

-- https://www.reddit.com/r/dailyprogrammer/comments/5hy8sm/20161212_challenge_295_easy_letter_by_letter/

import Data.List
import Data.Char

main :: IO ()
main =  result >>= putStrLn.unlines
  where
    result = takeAndSwapOne <$> getLine <*> getLine
    takeAndSwapOne xs ys = nub $ map (\n -> take n ys ++ drop n xs) [0 .. length xs]
