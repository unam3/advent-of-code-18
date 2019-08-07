import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List


type MarbleNumber = Int
type Circle = [MarbleNumber]
type CurrentMarblePosition = Int
type MarbleToBePlaced = MarbleNumber

type PlayerNumber = Int
type PlayersNumber = Int
type LastMarbleWorth = Int
type HighScore = Maybe Int
type TerminalMarble = Int

parseInput :: String -> (PlayersNumber, LastMarbleWorth, HighScore)
parseInput = parseStringsList . words

parseStringsList :: [String] -> (PlayersNumber, LastMarbleWorth, HighScore)
--["431","players;","last","marble","is","worth","70950","points"]
--["10","players;","last","marble","is","worth","1618","points:","high","score","is","8317"]
parseStringsList (playersNumber:_:_:_:_:_:lastMarbleWorth:_:xs) = (read playersNumber, read lastMarbleWorth, highScore) where
    highScore = if null xs
        then Nothing
        else Just (read $ xs !! 3)
parseStringsList _ = error "check your input"

type Score = Map.Map PlayerNumber [MarbleNumber]

getNextPlayerNumber :: PlayersNumber -> PlayerNumber -> PlayerNumber
getNextPlayerNumber playersNumber playerNumber = if playerNumber == playersNumber
    then 1
    else playerNumber + 1

takeTurn :: PlayersNumber -> TerminalMarble -> Circle -> PlayerNumber -> CurrentMarblePosition -> MarbleToBePlaced -> Score -> (Circle, Score)
takeTurn playersNumber terminalMarble circle playerNumber currentMarblePosition marbleToBePlaced score = 
    if terminalMarble == marbleToBePlaced
        then (circle, score)
        else
            if mod marbleToBePlaced 23 == 0
                then
                    let {
                        difference = currentMarblePosition - 7;
                        canSplitWithoutOverlapingCircle = difference > 0;
                        positionBeforeSplit = if canSplitWithoutOverlapingCircle
                            then difference
                            else length circle + difference;
                        (left, right) = splitAt positionBeforeSplit circle;
                        marbleToBePlacedBeforePosition = length left;
                        emptyRight = difference == 0;
                        removedMarbleNumber = if emptyRight
                            then last circle
                            else head right;
                        newCircle = if emptyRight
                            then init left
                            else left ++ tail right;
                        newScore = Map.insertWith (++) playerNumber [marbleToBePlaced, removedMarbleNumber] score;
                    } in takeTurn playersNumber terminalMarble newCircle nextPlayerNumber marbleToBePlacedBeforePosition (marbleToBePlaced+1) newScore
                    
                else let {
                    -- 1 is first element index because of splitAt
                    canPlaceToTheRight = length circle - (currentMarblePosition + 1) > 0;
                    marbleToBePlacedBeforePosition = if canPlaceToTheRight
                        then currentMarblePosition + 2
                        else 1;
                    (left, right) = splitAt marbleToBePlacedBeforePosition circle;
                } in takeTurn playersNumber terminalMarble (left ++ [marbleToBePlaced] ++ right) nextPlayerNumber marbleToBePlacedBeforePosition (marbleToBePlaced+1) score where
                    nextPlayerNumber = getNextPlayerNumber playersNumber playerNumber

    
getHighScore :: Score -> Int
getHighScore = sum . snd . last . (List.sortOn (sum . snd)) . Map.toList

getWinningScore :: (PlayersNumber, LastMarbleWorth, HighScore) -> Int
-- getWinningScore :: (PlayersNumber, LastMarbleWorth, HighScore) -> (Circle, Score)
getWinningScore (playersNumber, lastMarbleWorth, _) =
     getHighScore . snd $ takeTurn playersNumber terminalMarble initialCircle playerNumber currentMarblePosition marbleToBePlaced score where
     --takeTurn playersNumber terminalMarble initialCircle playerNumber currentMarblePosition marbleToBePlaced score where
        initialCircle = [0, 2, 1] :: Circle
        terminalMarble = lastMarbleWorth + 1
        currentMarblePosition = 1
        marbleToBePlaced = 3
        playerNumber = 3
        score = Map.empty


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

          solveFirstPuzzlePart = show . getWinningScore . parseInput

          --solveSecondPuzzlePart input = show $ findRootValue (map (read :: String -> Int) $ words $ head $ lines input)

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
              -- ++ "Second part solution is: " ++ solveSecondPuzzlePart input

{-
-}

