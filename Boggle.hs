module Boggle (boggle) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

{--
    Fill in the boggle function below. Use as many helpers as you want.
    Test your code by running 'cabal test' from the tester_hs_simple directory.
--}

isValid :: [(Int, Int)] -> Bool
isValid items =
    if ((length items == 1) && (fst (head items) == -1)) then False
    else True

getNeighbors :: Int -> Int -> Int -> [(Int, Int)]
getNeighbors i j board_size =
    let possible_neighbors = [(a + i, b + j) | a <- [-1,0,1], b <- [-1,0,1], (((a,b) /= (0,0)) && ((a+i) >= 0) && ((a+i) < board_size) && ((b+j) >= 0) && ((b+j) < board_size))]
    in possible_neighbors

getValidNeighbors :: Int -> Int -> [[Char]] -> Char -> Set (Int, Int) -> Int -> [(Int, Int)]
getValidNeighbors i j board next_char visited_points board_size =
    filter (\(x,y) -> ((((board !! x) !! y) == next_char) && (Set.notMember (x,y) visited_points))) (getNeighbors i j board_size)

findWord :: String -> [[Char]] -> (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
findWord "" _board _point _visited_points points_list _board_size = reverse points_list

findWord (next_char:next_word) board (i, j) visited_points points_list board_size =
    let valid_neighbors = getValidNeighbors i j board next_char visited_points board_size
        tryNeighbor (x, y) =
            let result = findWord next_word board (x, y) (Set.insert (x, y) visited_points) ((x, y) : points_list) board_size
            in if result /= [(-1, -1)] then result else []
        found = filter (/= []) (map tryNeighbor valid_neighbors)
    in if null found then [(-1, -1)] else head found

searchForWords :: [String] -> [String] -> [[Char]] -> (Int, Int) -> Map String [(Int, Int)] -> Int -> ([String], Map String [(Int, Int)])
searchForWords [] new_dict_arr _board _point found_dict _board_size = (new_dict_arr, found_dict)

searchForWords dict_arr new_dict_arr board point found_dict board_size =
    let search_result = (findWord (tail (head dict_arr)) board point (Set.singleton point) [point] board_size)
    in
        if (isValid search_result) then (searchForWords (tail dict_arr) new_dict_arr board point (Map.insert (head dict_arr) search_result found_dict) board_size)
        else (searchForWords (tail dict_arr) ((head dict_arr) : new_dict_arr) board point found_dict board_size)

searchBoard :: Map Char [[Char]] -> [[Char]] -> (Int, Int) -> Map String [(Int, Int)] -> Int -> Map String [(Int, Int)]
searchBoard word_dict board (i, j) found_dict board_size
    | (i >= board_size) = found_dict
    | (j >= board_size) = (searchBoard word_dict board (i+1,0) found_dict board_size)
    | otherwise =
        let curr_char = ((board !! i) !! j)
        in
            if (Map.member curr_char word_dict) then
                let (new_dict_arr, new_found_dict) = (searchForWords (Map.findWithDefault [] curr_char word_dict) [] board (i, j) found_dict board_size)
                in (searchBoard (Map.insert curr_char new_dict_arr word_dict) board (i,j+1) new_found_dict board_size)
            else (searchBoard word_dict board (i,j+1) found_dict board_size)

boggle :: [[Char]] -> [[Char]] -> [ (String, [ (Int, Int) ] ) ]
boggle board words =
    let
        board_size = (length board)
        board_full_size = (board_size * board_size)
        valid_words = filter (\word -> (length word <= board_full_size)) words
        word_dict =
            foldr (\word acc ->
                let key = (head word)
                in Map.insertWith (++) key [word] acc
                ) Map.empty valid_words
        found_dict = (searchBoard word_dict board (0,0) Map.empty board_size)
    in Map.toList found_dict