import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
--import qualified Data.Maybe as Maybe

type Step = Char
type Dependencies = [Step]
type StepsTaken = [Step]

type State = (StepsTaken, InstructionStepsToProcess)

type InstructionStepsToProcess = Map.Map Step Dependencies

-- Step - dependencies, which key step resolves
-- E is absent in keys
-- fromList [('A',"BD"),('B',"E"),('C',"AF"),('D',"E"),('F',"E")]
makeInstructionStepsToProcess :: (Step, Step) -> InstructionStepsToProcess -> InstructionStepsToProcess
makeInstructionStepsToProcess (step, whichDependencyResolves) = Map.insertWith (++) step [whichDependencyResolves]

-- Step - dependencies steps to take key step
-- C is absent in keys
-- fromList [('A',"C"),('B',"A"),('D',"A"),('E',"BDF"),('F',"C")]
makeInstructionStepsToProcess' :: (Step, Step) -> InstructionStepsToProcess -> InstructionStepsToProcess
makeInstructionStepsToProcess' (dependency, step) = Map.insertWith (++) step [dependency]

getStep :: [String] -> Step
getStep = head . (!! 1)

getDependencyToResolveStep :: [String] -> Step
getDependencyToResolveStep = head . (!! 7)

strToStep_DependencyToStep :: [String] -> (Step, Step)
strToStep_DependencyToStep strList = (getStep strList, getDependencyToResolveStep strList)

preprocessInstructionSteps :: String -> InstructionStepsToProcess
preprocessInstructionSteps str = foldr makeInstructionStepsToProcess' (makePartialStepsMap str) (listOfTuples str) where

    makePartialStepsMap :: String -> InstructionStepsToProcess
    makePartialStepsMap = Map.fromList . makeTuplesForMap . partialListOfSteps . (map $ getStep . words) . lines

    makeTuplesForMap :: [Step] -> [(Step, Dependencies)]
    makeTuplesForMap = map (\step -> (step, ""))

    partialListOfSteps :: [Step] -> [Step]
    partialListOfSteps = List.nub . List.foldr (:) []

    listOfTuples :: String -> [(Step, Step)]
    listOfTuples = map makeTuple . lines

    makeTuple = strToStep_DependencyToStep . words

whichStepToTake :: InstructionStepsToProcess -> Maybe Step
whichStepToTake instructionStepsToProcess = case Map.keys $ Map.filter (==  "") instructionStepsToProcess of
    [] -> Nothing
    step -> Just (head step)

takeStep :: State -> Step -> State
takeStep (stepsTaken, instructionStepsToProcess) stepToTake =
    (stepToTake : stepsTaken, updatedInstructionStepsToProcessWithTakenStep) where
        instructionStepsToProcessWithTakenStep = Map.delete stepToTake instructionStepsToProcess
        updatedInstructionStepsToProcessWithTakenStep = Map.map (List.delete stepToTake) instructionStepsToProcessWithTakenStep

determineOrderOfSteps :: State -> State
determineOrderOfSteps state@(_, instructionSteps) = case whichStepToTake instructionSteps of
    Just stepToTake -> determineOrderOfSteps $ takeStep state stepToTake
    Nothing -> state

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

          solveFirstPuzzlePart input = show . List.reverse . fst . determineOrderOfSteps $ ([], preprocessInstructionSteps input)

          -- solveSecondPuzzlePart input =  where

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
            -- ++ "Second part solution is: " ++ solveSecondPuzzlePart input

{-
You find yourself standing on a snow-covered coastline; apparently, you landed a little off course. The region is too hilly to see the North Pole from here, but you do spot some Elves that seem to be trying to unpack something that washed ashore. It's quite cold out, so you decide to risk creating a paradox by asking them for directions.

"Oh, are you the search party?" Somehow, you can understand whatever Elves from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the device on your wrist also be a translator? "Those clothes don't look very warm; take this." They hand you a heavy coat.

"We do need to find our way back to the North Pole, but we have higher priorities at the moment. You see, believe it or not, this box contains something that will solve all of Santa's transportation problems - at least, that's what it looks like from the pictures in the instructions." It doesn't seem like they can read whatever language it's in, but you can: "Sleigh kit. Some assembly required."

"'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!" They start excitedly pulling more parts out of the box.

The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input). Each step is designated by a single letter. For example, suppose you have the following instructions:

Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
Visually, these requirements look like this:


  -->A--->B--
 /    \      \
C      -->D----->E
 \           /
  ---->F-----
Your first goal is to determine the order in which the steps should be completed. If more than one step is ready, choose the step which is first alphabetically. In this example, the steps would be completed as follows:

Only C is available, and so it is done first.
Next, both A and F are available. A is first alphabetically, so it is done next.
Then, even though F was available earlier, steps B and D are now also available, and B is the first alphabetically of the three.
After that, only D and F are available. E is not available because only some of its prerequisites are complete. Therefore, D is completed next.
F is the only choice, so it is done next.
Finally, E is completed.
So, in this example, the correct order is CABDFE.

In what order should the steps in your instructions be completed?
-}
