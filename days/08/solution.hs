import System.Environment (getArgs)
import qualified Data.List as List

-- metadataEntriesQtt - after the children in current branch
treeMetadataEntriesSum :: [Int] -> (Int, [Int])
treeMetadataEntriesSum (childNodesQtt : metadataEntriesQtt : numbersListRest) =
    let (childrenMetadataSum, etc) = getSum childNodesQtt metadataEntriesQtt numbersListRest
    in case etc of
        [] -> (childrenMetadataSum, etc)
        _ -> (-1, etc)
treeMetadataEntriesSum _ = (0, [])

type Sum = Int
type NumbersList = [Int]

getSum :: Int -> Int -> NumbersList -> (Sum, NumbersList)
getSum siblingsToProcessCount parentMetadataEntriesQuantity (childNodesQuantity:metadataEntriesQtt:numbersList) =
    case siblingsToProcessCount of
        1 -> case childNodesQuantity of
            -- testInput1
            0 -> (sum metadata + sum parentMetadata, numbersListAfterParentMetadata) where
                (metadata, numbersListRest) = splitAt metadataEntriesQtt numbersList
                (parentMetadata, numbersListAfterParentMetadata) = splitAt parentMetadataEntriesQuantity numbersListRest
            -- 3, 31
            _ -> (metadata + sum parentMetadata, childRest) where
                (metadata, numbersListRest) = getSum childNodesQuantity metadataEntriesQtt numbersList
                (parentMetadata, childRest) = splitAt parentMetadataEntriesQuantity numbersListRest
        _ -> case childNodesQuantity of
            -- 2, 21
            0 -> (sum metadata + siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) where
                (metadata, numbersListRest) = splitAt metadataEntriesQtt numbersList
                (siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) =
                    getSum (siblingsToProcessCount - 1) parentMetadataEntriesQuantity numbersListRest
            -- 4, 41
            _ -> (metadata + siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) where
                (metadata, afterThisNodeAndItsChildrenNumbersListRest) =
                    getSum childNodesQuantity metadataEntriesQtt numbersList
                (siblingsToTheRightMetadataSum, siblingsToTheRightNumbersListRest) =
                    getSum (siblingsToProcessCount - 1) parentMetadataEntriesQuantity afterThisNodeAndItsChildrenNumbersListRest
getSum _ _ _ = (0, [])

-- metadata entries
type Metadata = [Int]
type NodeValue = Int

findRootValue :: NumbersList -> (NodesToProcess, NumbersList, NodeValue)
findRootValue numbersList@(_ : _ : _) = 
    getNodeValue childNodesQtt metadataEntriesQtt (numbersList ++ metadata) firstNodeIndex childrenToProcess where
        childNodesQtt = 1 -- on the first level we can have only one node
        metadataEntriesQtt = 1
        metadata = [1]
        firstNodeIndex = 1
        childrenToProcess = []
findRootValue _ = ([], [], -1)


getMetadata :: Int -> Int -> NumbersList -> (Metadata, NumbersList)

getMetadata 0 parentMetadataEntriesQtt rest = splitAt parentMetadataEntriesQtt rest

getMetadata siblingsToProcessQtt parentMetadataEntriesQtt (childNodesQtt:metadataEntriesQtt:numbersList) =
    case siblingsToProcessQtt of
        1 -> case childNodesQtt of
            -- testInput1
            0 -> (
                    --[childNodesQtt, metadataEntriesQtt] ++ metadataAndParendMetadata,
                    parentMetadata,
                    numbersListAfterParentMetadata
                ) where
                --(metadataAndParendMetadata, numbersListAfterParentMetadata) =
                --    splitAt (metadataEntriesQtt + parentMetadataEntriesQtt) numbersList
                (parentMetadata, numbersListAfterParentMetadata) =
                    splitAt parentMetadataEntriesQtt $ drop metadataEntriesQtt numbersList
            -- 3, 31
            _ -> (parentMetadata, childRest) where
                (_, numbersListRest) = getMetadata childNodesQtt metadataEntriesQtt numbersList
                (parentMetadata, childRest) = splitAt parentMetadataEntriesQtt numbersListRest
        _ -> case childNodesQtt of
            -- 2, 21
            0 -> (
                    --[childNodesQtt, metadataEntriesQtt] ++ metadata ++ restTranssform,
                    parentMetadata,
                    numbersListAfterSibling
                ) where
                --(metadata, numbersListRest) = splitAt metadataEntriesQtt numbersList
                (parentMetadata, numbersListAfterSibling) =
                    getMetadata (siblingsToProcessQtt - 1) parentMetadataEntriesQtt $
                        drop metadataEntriesQtt numbersList
            -- 4, 41
            _ -> (parentMetadata, siblingsToTheRightNumbersListRest) where
                (_, afterThisNodeAndItsChildrenNumbersListRest) =
                    getMetadata childNodesQtt metadataEntriesQtt numbersList
                (parentMetadata, siblingsToTheRightNumbersListRest) =
                    getMetadata (siblingsToProcessQtt - 1) parentMetadataEntriesQtt afterThisNodeAndItsChildrenNumbersListRest
getMetadata _ _ _ = ([], [])


type ChildIndex = Int
type Count = Int
type NodesToProcess = [(ChildIndex, Count)]

processMetadata :: Int -> Metadata -> NodesToProcess
processMetadata unprocessedNodesCount = List.map (\x -> (head x, length x))
    . List.group
    -- A metadata entry of 0 does not refer to any child node.
    -- If a referenced child node does not exist, that reference is skipped.
    . List.filter (\index -> index > 0 && index <= unprocessedNodesCount)
    . List.sort 

getChildlessNodeValue :: Int -> NumbersList -> NodesToProcess -> (NodesToProcess, NumbersList, NodeValue)
getChildlessNodeValue metadataEntriesQtt numbersList nodesToProcessArg =
    (\(metadata, numbersListRest) -> (nodesToProcessArg, numbersListRest, sum metadata)) $
        splitAt metadataEntriesQtt numbersList

getNodeValue :: Int -> Int -> NumbersList -> Int -> NodesToProcess -> (NodesToProcess, NumbersList, NodeValue)
getNodeValue unprocessedNodesCount parentMetadataEntriesQtt rest@(childNodesQtt:metadataEntriesQtt:numbersList) index nodesToProcessArg = valueAndNumbersList where
    hasNoChildren = childNodesQtt == 0
    --noNodesToProcess = unprocessedNodesCount == 0
    nodesToProcess = if index == 1
        then processMetadata unprocessedNodesCount . fst $
            getMetadata unprocessedNodesCount parentMetadataEntriesQtt rest
        else nodesToProcessArg

    -- pass next element technique:
    -- getMetadata 1 5 [0, 1, 9,0,1,7,1,8]
    -- ([0,1,7,1,8],[])
    numbersListRestIfNoSuchIndex = snd $ getMetadata childNodesQtt metadataEntriesQtt numbersList
    nextNodesToProcess multiplier = List.delete (index, multiplier) nodesToProcess
    hasUnprocessedNodesAhead = (unprocessedNodesCount - 1) > 0
    --nextNodesToProcess = List.filter (\x -> (/= index) . fst) nodesToProcess

    valueAndNumbersList = case lookup index nodesToProcess of
        Nothing -> if hasUnprocessedNodesAhead
            then getNodeValue (unprocessedNodesCount-1) parentMetadataEntriesQtt numbersListRestIfNoSuchIndex (index+1) nodesToProcess
            --then (nodesToProcess, rest, "-7")
            --then (nodesToProcess, rest, show unprocessedNodesCount)
            else ([], [], 0)
        Just multiplier -> (\(nodesToProcess', numbersList', nodeValue':nodeValueRight') ->
                (
                    nodesToProcess',
                    numbersList',
                    if null nodeValueRight'
                        then if hasNoChildren
                            then nodeValue'
                            else multiplier * nodeValue'
                        else if hasNoChildren
                            then nodeValue' + head nodeValueRight'
                            else multiplier * nodeValue' + head nodeValueRight'
                )
            ) $ if hasNoChildren
                then if hasUnprocessedNodesAhead
                    --then (nodesToProcess, numbersList, "-3")
                    then (\(x, y, z) -> (x, y, [head ifChildlessNodeValue, z])) $
                        getNodeValue (unprocessedNodesCount-1) parentMetadataEntriesQtt ifChildlessNumbersList (index+1) ifChildlessNodesToProcess
                    --else ([], [-3], "-33")
                    else ifChildlessResults
                else if hasUnprocessedNodesAhead
                    --then (nodesToProcess, (-4:rest), "-4")
                    then (\(x, y, z) -> (x, y, [childNodeValue, z])) $
                        getNodeValue (unprocessedNodesCount-1) parentMetadataEntriesQtt numbersListRestIfNoSuchIndex (index+1) (nextNodesToProcess multiplier)

                    else (\(x, y, z) -> (x, y, [z])) childNodeResults where
                    --else (nodesToProcess, [index], "-44")
                    --else (nodesToProcess, numbersList, "-44")
            ifChildlessResults@(ifChildlessNodesToProcess, ifChildlessNumbersList, ifChildlessNodeValue) =
                (\(nodesToProcess', numbersList', nodeValue') ->
                    (nodesToProcess', numbersList', [multiplier * nodeValue'])) $
                        getChildlessNodeValue metadataEntriesQtt numbersList (nextNodesToProcess multiplier)
            childNodeResults@(_, _, childNodeValue) =
                getNodeValue childNodesQtt metadataEntriesQtt numbersList 1 []


getNodeValue _ _ numbersList@[1] _ nodesToProcessArg = (nodesToProcessArg, numbersList, 0)

--getNodeValue _ _ numbersList index nodesToProcessArg = (nodesToProcessArg, numbersList, "-2: " ++ show index)
--getNodeValue _ _ numbersList _ nodesToProcessArg = (nodesToProcessArg, numbersList, "wtf")
getNodeValue _ _ numbersList _ nodesToProcessArg = (nodesToProcessArg, numbersList, -10)

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
            show $ treeMetadataEntriesSum (map (read :: String -> Int) $ words $ head $ lines input)

          solveSecondPuzzlePart input = show $ findRootValue (map (read :: String -> Int) $ words $ head $ lines input)

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
              ++ "Second part solution is: " ++ solveSecondPuzzlePart input

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

--- Part Two ---

The second check is slightly more complicated: you need to find the value of the root node (A in the example above).

The value of a node depends on whether it has child nodes.

If a node has no child nodes, its value is the sum of its metadata entries. So, the value of node B is 10+11+12=33, and the value of node D is 99.

However, if a node does have child nodes, the metadata entries become indexes which refer to those child nodes. A metadata entry of 1 refers to the first child node, 2 to the second, 3 to the third, and so on. The value of this node is the sum of the values of the child nodes referenced by the metadata entries. If a referenced child node does not exist, that reference is skipped. A child node can be referenced multiple time and counts each time it is referenced. A metadata entry of 0 does not refer to any child node.

For example, again using the above nodes:

Node C has one metadata entry, 2. Because node C has only one child node, 2 references a child node which does not exist, and so the value of node C is 0.
Node A has three metadata entries: 1, 1, and 2. The 1 references node A's first child node, B, and the 2 references node A's second child node, C. Because node B has a value of 33 and node C has a value of 0, the value of node A is 33+33+0=66.
So, in this example, the value of the root node is 66.

What is the value of the root node?
-}
