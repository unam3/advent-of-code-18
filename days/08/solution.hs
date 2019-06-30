import System.Environment (getArgs)
--import qualified Data.List as List
--import qualified Data.Map.Strict as Map
--import qualified Data.Maybe as Maybe

-- metadataEntriesQuantity - after the children in current branch
treeMetadataEntriesSum :: [Int] -> (Int, [Int])
treeMetadataEntriesSum (childNodesQtt : metadataEntriesQuantity : numbersListRest) =
    let (childrenMetadataSum, etc) = getSum childNodesQtt metadataEntriesQuantity numbersListRest
    in case etc of
        [] -> (childrenMetadataSum, etc)
        _ -> (-1, etc)
treeMetadataEntriesSum _ = (0, [])

-- siblingsToProcessCount - to process
getSum :: Int -> Int -> [Int] -> (Int, [Int])
getSum siblingsToProcessCount parentMetadataEntriesQuantity (childNodesQuantity:metadataEntriesQuantity:numbersList) =
    case siblingsToProcessCount of
        1 -> case childNodesQuantity of
            -- (1)
            0 -> (sum metadata + sum parentMetadata, numbersListAfterParentMetadata) where
                (metadata, numbersListRest) = splitAt metadataEntriesQuantity numbersList
                (parentMetadata, numbersListAfterParentMetadata) = splitAt parentMetadataEntriesQuantity numbersListRest
            -- (3, 31)
            _ -> (metadata + sum parentMetadata, childRest) where
                (metadata, numbersListRest) = getSum childNodesQuantity metadataEntriesQuantity numbersList
                (parentMetadata, childRest) = splitAt parentMetadataEntriesQuantity numbersListRest
        _ -> case childNodesQuantity of
            -- (2, 21)
            0 -> (sum metadata + siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) where
                (metadata, numbersListRest) = splitAt metadataEntriesQuantity numbersList
                (siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) =
                    getSum (siblingsToProcessCount - 1) parentMetadataEntriesQuantity numbersListRest
            -- (4)
            _ -> (metadata + siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) where
                (metadata, afterThisNodeAndItsChildrenNumbersListRest) =
                    getSum childNodesQuantity metadataEntriesQuantity numbersList
                (siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) =
                    getSum (siblingsToProcessCount - 1) parentMetadataEntriesQuantity afterThisNodeAndItsChildrenNumbersListRest


interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input

main :: IO ()
main = mainWith solvePuzzle
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          solveFirstPuzzlePart input =
            show $ treeMetadataEntriesSum (map (read :: String -> Int) $ words input)

          --solveSecondPuzzlePart input = show where

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
            -- ++ "Second part solution is: " ++ solveSecondPuzzlePart input

{-
--- Day 8: Memory Maneuver ---

The sleigh is much easier to pull than you'd expect for something its weight. Unfortunately, neither you nor the Elves know which way the North Pole is from here.

You check your wrist device for anything that might help. It seems to have some kind of navigation system! Activating the navigation system produces more bad news: "Failed to start navigation system. Could not read software license file."

The navigation system's license file consists of a list of numbers (your puzzle input). The numbers define a data structure which, when processed, produces some kind of tree that can be used to calculate the license number.

The tree is made up of nodes; a single, outermost node forms the tree's root, and it contains all other nodes in the tree (or contains nodes that contain nodes, and so on).

Specifically, a node consists of:

A header, which is always exactly two numbers:
The quantity of child nodes.
The quantity of metadata entries.
Zero or more child nodes (as specified in the header).
One or more metadata entries (as specified in the header).
Each child node is itself a node that has its own header, child nodes, and metadata. For example:

2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----
In this example, each node of the tree is also marked with an underline starting with a letter for easier identification. In it, there are four nodes:

A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
C, which has 1 child node (D) and 1 metadata entry (2).
D, which has 0 child nodes and 1 metadata entry (99).
The first check done on the license file is to simply add up all of the metadata entries. In this example, that sum is 1+1+2+10+11+12+2+99=138.

What is the sum of all metadata entries?
-}
