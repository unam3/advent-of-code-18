import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Sequence as Seq


type MarbleNumber = Integer
type Circle = Seq.Seq MarbleNumber
type CurrentMarblePosition = Integer
type MarbleToBePlaced = MarbleNumber

type PlayerNumber = Int
type PlayersNumber = Int
type LastMarbleWorth = Int
type HighScore = Maybe Integer
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
takeTurn playersNumber terminalMarble circle playerNumber currentMarblePosition marbleToBePlaced score
    | toInteger terminalMarble == marbleToBePlaced = (circle, score)
    | mod marbleToBePlaced 23 == 0 =
        let {
            difference = fromInteger currentMarblePosition - 7 :: Int;
            canSplitWithoutOverlapingCircle = difference > 0;
            positionBeforeSplit = if canSplitWithoutOverlapingCircle
                then difference
                else Seq.length circle + difference;
            (left, right) = Seq.splitAt positionBeforeSplit circle;
            marbleToBePlacedBeforePosition = toInteger $ Seq.length left;
            emptyRight = difference == 0;
            removedMarbleNumber = if emptyRight
                then Seq.index circle $ Seq.length circle - 1
                else Seq.index right 0;
            newCircle = if emptyRight
                then Seq.take (Seq.length circle - 2) left
                else left Seq.>< Seq.drop 1 right;
            newScore = Map.insertWith (++) playerNumber [marbleToBePlaced, removedMarbleNumber] score;
        } in difference `seq` positionBeforeSplit `seq` marbleToBePlacedBeforePosition `seq` removedMarbleNumber `seq` newCircle `seq` newScore `seq` (takeTurn playersNumber terminalMarble newCircle nextPlayerNumber marbleToBePlacedBeforePosition (marbleToBePlaced+1) newScore)
            
    | otherwise = let {
        -- 1 is first element index because of splitAt
        canPlaceToTheRight = length circle - (fromInteger $ currentMarblePosition + 1) > 0;
        marbleToBePlacedBeforePosition = if canPlaceToTheRight
            then currentMarblePosition + 2
            else 1;
        (left, right) = Seq.splitAt (fromInteger marbleToBePlacedBeforePosition) circle;
    } in takeTurn playersNumber terminalMarble (left Seq.>< Seq.singleton marbleToBePlaced Seq.>< right) nextPlayerNumber marbleToBePlacedBeforePosition (marbleToBePlaced+1) score
    where nextPlayerNumber = getNextPlayerNumber playersNumber playerNumber

    
getHighScore :: Score -> Integer
getHighScore = toInteger . sum . snd . last . List.sortOn (sum . snd) . Map.toList

getWinningScore ::  Int -> (PlayersNumber, LastMarbleWorth, HighScore) -> Integer
getWinningScore multiplier (playersNumber, lastMarbleWorth, _) =
     getHighScore . snd $ takeTurn playersNumber terminalMarble initialCircle playerNumber currentMarblePosition marbleToBePlaced score where
     --takeTurn playersNumber terminalMarble initialCircle playerNumber currentMarblePosition marbleToBePlaced score where
        initialCircle = Seq.fromList [0, 2, 1] :: Circle
        terminalMarble = lastMarbleWorth * multiplier + 1
        currentMarblePosition = 1 :: Integer
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

          solveFirstPuzzlePart = show . getWinningScore 1 . parseInput
          solveSecondPuzzlePart = show . getWinningScore 100 . parseInput


          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
               ++ "Second part solution is: " ++ solveSecondPuzzlePart input

{-
-}

