module Reddit where


-- https://www.reddit.com/r/dailyprogrammer/comments/5ijb4z/20161215_challenge_295_intermediate_seperated/

import Data.List
import Data.Char

--This Makes the answer
howManyTables :: Int -> Int
howManyTables = length.tables

countValidTableLayouts :: Int -> Int
countValidTableLayouts n = div (howManyTables n) (n*2)

validTableLayouts :: String -> [String]
validTableLayouts = validTableLayouts' True

validTableLayouts' :: Bool -> String -> [String]
validTableLayouts' circular lowerPartners = if circular then meetSomeoneLayouts else removeCircles
  where
    upperPartners = map toUpper lowerPartners
    allPeople = lowerPartners ++ upperPartners
    meetSomeoneLayouts = filter (not.validTable) $ permutations allPeople
    removeCircles = nubBy isRotation meetSomeoneLayouts

isRotation:: (Eq a) => [a] -> [a] -> Bool
isRotation x y =  y `isInfixOf` (x++x)

tables :: Int -> [String]
tables n = validTableLayouts $ take n ['a'..'z']

validTable :: String -> Bool
validTable = adjDub.bendTable

bendTable :: String -> String
bendTable layout =  map toUpper $ layout ++ [head layout]

adjDub :: (Eq a) => [a] -> Bool
adjDub [] = False
adjDub [_] = False
adjDub (x1 : x2 : xs) =  if x1 == x2 then True else adjDub (x2 : xs)


howManyFixes :: String -> Int
howManyFixes = length.fixTheTable

fixTheTable :: String -> [String]
fixTheTable start = removeCircles
  where
    missing = findMissing start
    fixedSeating = [ mergeWithWild start missingOrder | missingOrder <- permutations missing]
    meetSomeoneLayouts = filter (not.validTable) fixedSeating
    removeCircles = nubBy isRotation meetSomeoneLayouts


mergeWithWild :: String -> String -> String
mergeWithWild [s] [] = [s]
mergeWithWild (s:start) (m:missing) = if s == '_' then m : mergeWithWild start missing
                                                  else s : mergeWithWild start (m:missing)
mergeWithWild _ _ = []

findMissing :: String -> String
findMissing start = missing
  where
    mirror = [if isUpper c then toLower c else toUpper c | c <- start]
    missing = mirror \\ start


-- main :: IO ()
-- main =  result >>= putStrLn.unlines
--   where
--     result = takeAndSwapOne <$> getLine <*> getLine
--     takeAndSwapOne xs ys = nub $ map (\n -> take n ys ++ drop n xs) [0 .. length xs]
