import System.Environment (getArgs)
--import qualified Data.List as List
--import qualified Data.Ord as Ord
import qualified Data.Map.Strict as Map
--import qualified Data.Maybe as Maybe

type Step = Char
type Dependencies = [Step]
type DependencyTo = [Step]

--type InstructionStepsToProcess = Map.Map Step (Dependencies, DependencyTo)
--type State = Map stepsTaken (Dependencies, DependencyTo)

type InstructionStepsToProcess = Map.Map Step DependencyTo

makeInstructionStepsToProcess :: (Step, Step) -> InstructionStepsToProcess -> InstructionStepsToProcess
makeInstructionStepsToProcess (k, v) = Map.insertWith (++) k [v]

preprocessInstructionSteps :: String -> InstructionStepsToProcess
--preprocessInstructionSteps :: String -> InstructionsToProcess
preprocessInstructionSteps str = foldr makeInstructionStepsToProcess Map.empty (listOfTuples str) where
    listOfTuples :: String -> [(Step, Step)]
    listOfTuples = map makeTuple . lines
    makeTuple = strToStep_DependencyToStep . words

strToStep_DependencyToStep :: [String] -> (Step, Step)
strToStep_DependencyToStep strList = (head $ strList !! 1, head $ strList !! 7)

--findStepToTake :: State -> Step

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

          solveFirstPuzzlePart = show . preprocessInstructionSteps where

          -- solveSecondPuzzlePart input =  where

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
            -- ++ "Second part solution is: " ++ solveSecondPuzzlePart input
