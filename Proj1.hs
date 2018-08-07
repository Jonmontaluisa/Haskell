-- Jonathan Montaluisa
-- ID 743235
-- The present program is a Project for Declarative Programming COMP 30020 from
-- The University of Melbourne

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import System.Random
import System.IO.Unsafe

-- Game State will carry all the probable values for a Chord and then used to
-- create a new GameState from previos feedback
type GameState =   [[String]] 


pitchList = [i:j | i <- ['A'..'G'] , j <-  ["1","2","3"]]

-- All the probable chord combination. 1330 in total
chordList = combinations 3 pitchList

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
-- initial Guess is hard coded
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C3"], (chordList))


nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (a, b) (c, d, e)  =  (newGameState!!((randNum nGS_length)-1), 
 newGameState)
  where
   newGameState = gameStateFilter (a, b) (c, d, e)
   nGS_length =  length newGameState 

-- produce a randon number in a range 
randNum :: Int -> Int
randNum a = unsafePerformIO (getStdRandom (randomR (1, a)))
 
-- main function that deletes all not necesary chord given the tuple (Int Int int)
gameStateFilter :: ([String], GameState) -> (Int, Int, Int) -> [[String]]
gameStateFilter (a, b) (c, d, e) = 
 delete a (byOctList a (e+c) (byNoteList a (c+d) y))
  where 
   y = pitchFilter a c b

-- Use the first Int from the tuple (Int, Int,Int) to delete any chord that not 
-- satisfy a minim
pitchFilter :: [String]-> Int -> [[String]] -> [[String]]
pitchFilter xs a ys
 | a>= 1 = filter (compTwoList  a xs ) ys
 | otherwise = ys \\ filter (compTwoList  1 xs ) ys

-- Check if a list ys has a sublist from the combination of elements of list xs
compTwoList :: (Eq a) =>Int -> [a] ->  [a] -> Bool
compTwoList b xs ys = hasSublist (combinations  b xs) ys

-- Check if at least one of the elem from a list of lists  is contained in 
-- another list a. For instance:
-- hasSublist [[1,2],[5,6]] [1,2,3] will return true
-- hasSublist [[3,4],[5,6]] [1,2,3] will return false
hasSublist:: (Eq a) => [[a]]->[a]->Bool
hasSublist [] b = False
hasSublist (x:xs) ys
 | isSublist x ys = True
 | otherwise = hasSublist xs ys

isSublist:: (Eq a) => [a] -> [a] -> Bool
isSublist xs ys = null (xs\\ys)


-- Filter by comparing  the Notes of a chord with a list of chords 
byNoteList :: [String] -> Int -> [[String]] -> [[String]]
byNoteList sr1 b sr2 
 | b >= 1 = filter (compByNote sr1 b) sr2
 | b == 0 = sr2 \\ (filter (compByNote sr1 1) sr2)


-- Filter by comparing  the Pitch of a chord with a list of chords
byOctList :: [String] -> Int -> [[String]] -> [[String]]
byOctList sr1 b sr2 = filter (compByOct sr1 b) sr2

-- Take two chords and compare the notes. If the share a minimum number of notes
-- return True
compByNote :: [String] -> Int -> [String] -> Bool
compByNote sr1 b sr2 = 
	(3 - b) >= length ( (map (\x -> [head x]) sr1) \\ (map (\x->[head x]) sr2))

-- Take two chords and compare the octaves. If the share a minimum number of 
-- octaves return True
compByOct :: [String] -> Int -> [String] -> Bool
compByOct  sr1 b sr2 = 
	(3 - b) >= length ( (map (\x -> [x!!1]) sr1) \\ (map (\x-> [x!!1] ) sr2))

