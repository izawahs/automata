module Automata.Cells where

import Automata.Cells.Rules
import Automata.Cells.Operators
import Automata.Util (gen', gen3')
import Automata.Cells.Types


-- Automata

automata :: (Cell -> Cell -> Cell -> Cell) -> [Cell] -> [[Cell]]
automata f cs = iterate (\cs -> [D, D] ++ gen3' f cs ++ [D, D]) ([D, D] ++ gen3' f cs ++ [D, D])

automata' :: (Cell -> Cell -> Cell) -> [Cell] -> [[Cell]]
automata' f cs = iterate (\cs -> [D] ++ gen' f cs ++ [D]) ([D] ++ gen' f cs ++ [D])

autoL :: (Cell -> Cell -> Cell -> Cell) -> [Cell] -> [[Cell]]
autoL f cs = iterate (\cs -> [L, L] ++ gen3' f cs ++ [L, L]) cs

autoD :: (Cell -> Cell -> Cell -> Cell) -> [Cell] -> [[Cell]]
autoD f cs = iterate (\cs -> [D, D] ++ gen3' f cs ++ [D, D]) cs

autoLs :: (Cell -> Cell -> Cell -> Cell) -> Int -> [[Cell]]
autoLs f n = autoL f $ replicate n L

autoDs :: (Cell -> Cell -> Cell -> Cell) -> Int -> [[Cell]]
autoDs f n = autoD f $ replicate n D

-- Converters

convert :: a -> a -> [[Cell]] -> [[a]]
convert l d = map $ map (\c -> if c == L then l else d)

convertCh :: [[Cell]] -> [String]
convertCh = convert '*' ' '

-- Generators

generate :: (Cell -> Cell -> Cell -> Cell) -> Int -> IO ()
generate f x = mapM_ putStrLn $ convertCh $ take x $ automata f [D, L, D]

generate' :: (Cell -> Cell -> Cell) -> Int -> IO ()
generate' f x = mapM_ putStrLn $ convertCh $ take x $ automata' f [D, L, D]

squL :: (Cell -> Cell -> Cell -> Cell) -> Int -> IO ()
squL f n = mapM_ putStrLn $ convertCh $ take n $ autoLs f n

squD :: (Cell -> Cell -> Cell -> Cell) -> Int -> IO ()
squD f n = mapM_ putStrLn $ convertCh $ take n $ autoDs f n

