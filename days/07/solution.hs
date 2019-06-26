import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

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

whichStepsAvailable :: InstructionStepsToProcess -> Maybe [Step]
whichStepsAvailable instructionStepsToProcess = case Map.keys $ Map.filter (==  "") instructionStepsToProcess of
    [] -> Nothing
    steps -> Just steps

whichStepToTake :: Maybe [Step] -> Maybe Step
whichStepToTake (Just []) = Nothing
whichStepToTake (Just steps) = Just (head steps)
whichStepToTake _ = Nothing

takeStep :: State -> Step -> State
takeStep (stepsTaken, instructionStepsToProcess) stepToTake =
    (stepToTake : stepsTaken, updatedInstructionStepsToProcessWithTakenStep) where
        instructionStepsToProcessWithTakenStep = Map.delete stepToTake instructionStepsToProcess
        updatedInstructionStepsToProcessWithTakenStep =
            Map.map (List.delete stepToTake) instructionStepsToProcessWithTakenStep

determineOrderOfSteps :: State -> State
determineOrderOfSteps state@(_, instructionSteps) =
    case whichStepToTake $ whichStepsAvailable instructionSteps of
        Just stepToTake -> determineOrderOfSteps $ takeStep state stepToTake
        Nothing -> state


type NumberOfWorkers = Int
type TimeToCompleteSteps = Int
type RemainingTime = Int

type WorkInProgress = [(RemainingTime, Step)]
type ConcurrentState = (TimeToCompleteSteps, InstructionStepsToProcess, WorkInProgress)

assignStepsToWorkers :: NumberOfWorkers -> InstructionStepsToProcess -> WorkInProgress -> WorkInProgress
assignStepsToWorkers freeWorkersCount instructionStepsToProcess workInProgress
    | maybeStep /= Nothing =
        if freeWorkersCount > 1
            then assignStepsToWorkers (freeWorkersCount - 1) newInstructionStepsToProcess newWorkInProgress
            else newWorkInProgress
    | otherwise = workInProgress where
        newWorkInProgress = ((stepTakesTime, Maybe.fromJust maybeStep): workInProgress)
        maybeStep = whichStepToTake $ filterWIPSteps workInProgress (whichStepsAvailable instructionStepsToProcess)
        stepTakesTime = Maybe.fromJust $ maybeStep >>= getStepAdditionalTime 
        newInstructionStepsToProcess = Map.delete (Maybe.fromJust maybeStep) instructionStepsToProcess

filterWIPSteps :: WorkInProgress -> Maybe [Step] -> Maybe [Step]
filterWIPSteps workInProgress (Just stepsAvailable) =
    case List.filter (filterWorkInProgress workInProgress) stepsAvailable of
        filteredStepsAvailable@(_:_) -> Just filteredStepsAvailable
        [] -> Nothing
filterWIPSteps _ _ = Nothing

filterWorkInProgress :: WorkInProgress -> Step -> Bool
filterWorkInProgress workInProgress = (\availableStep -> List.notElem availableStep (List.map snd workInProgress))

--howLongWillItTake :: NumberOfWorkers -> ConcurrentState -> TimeToCompleteSteps
--howLongWillItTake :: NumberOfWorkers -> ConcurrentState -> WorkInProgress
howLongWillItTake :: NumberOfWorkers -> ConcurrentState -> ConcurrentState
howLongWillItTake numberOfWorkers (timeToCompleteSteps, instructionStepsToProcess, workInProgress) =
    let freeWorkersCount = numberOfWorkers - (length workInProgress)
        wipAfterAssignments = assignStepsToWorkers freeWorkersCount instructionStepsToProcess workInProgress
        newConcurrentState = (timeToCompleteSteps, instructionStepsToProcess, wipAfterAssignments)
    in case Map.null instructionStepsToProcess of
    --in case Map.null instructionStepsToProcess || (length $ Map.keys instructionStepsToProcess) == 28 of
        --True -> timeToCompleteSteps + 1
        --True -> wipAfterAssignments
        True -> newConcurrentState
        False -> howLongWillItTake numberOfWorkers (takeConcurrentStep newConcurrentState)

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

takeConcurrentStep :: ConcurrentState -> ConcurrentState
takeConcurrentStep (timeToCompleteSteps, instructionStepsToProcess, workInProgress) =
    (newTimeToCompleteSteps, newInstructionStepsToProcess, newWorkInProgress) where
        sortedWorkInProgress = sortByMinRemainingTime $ sortByStepName workInProgress
        tupleWithStepToTake = case sortedWorkInProgress of
            [] -> (0, 'Z')
            _ -> head sortedWorkInProgress
        timeToCompleteCurrentStep = fst tupleWithStepToTake
        newTimeToCompleteSteps = timeToCompleteCurrentStep + timeToCompleteSteps
        stepToTake = snd tupleWithStepToTake
        newInstructionStepsToProcess =
            Map.map (List.delete stepToTake) (Map.delete stepToTake instructionStepsToProcess)
        subtractTimeToCompleteCurrentStep = List.map $ \(time, step) -> (time - timeToCompleteCurrentStep, step)
        newWorkInProgress = subtractTimeToCompleteCurrentStep $ List.tail sortedWorkInProgress

stepToTimeTuples :: [(Char, Int)]
stepToTimeTuples = List.zip ['A'..'Z'] $ map (60 +) [1..26]
--stepToTimeTuples = List.zip ['A'..'Z'] [1..26]

getStepAdditionalTime :: Char -> Maybe Int
getStepAdditionalTime step = List.lookup step stepToTimeTuples

sortByStepName :: WorkInProgress -> WorkInProgress
sortByStepName = List.sortOn snd

sortByMinRemainingTime :: WorkInProgress -> WorkInProgress
sortByMinRemainingTime = List.sortOn fst


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
            show . List.reverse . fst . determineOrderOfSteps $ ([], preprocessInstructionSteps input)

          solveSecondPuzzlePart input = show $ howLongWillItTake numberOfWorkers initialConcurrentState where
          --solveSecondPuzzlePart input = howLongWillItTake 2 initialConcurrentState where
            numberOfWorkers = 5
            --numberOfWorkers = 2
            initialConcurrentState = (0, instructionStepsToProcess, [])
            instructionStepsToProcess = preprocessInstructionSteps input

          --solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
          solvePuzzle input = "Second part solution is: " ++ solveSecondPuzzlePart input
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

--- Part Two ---

As you're about to begin construction, four of the Elves offer to help. "The sun will set soon; it'll go faster if we work together." Now, you need to account for multiple people working on steps simultaneously. If multiple steps are available, workers should still begin them in alphabetical order.

Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds. No time is required between steps.

To simplify things for the example, however, suppose you only have help from one Elf (a total of two workers) and that each step takes 60 fewer seconds (so that step A takes 1 second and step Z takes 26 seconds). Then, using the same instructions as above, this is how each second would be spent:

Second   Worker 1   Worker 2   Done
   0        C          .        
   1        C          .        
   2        C          .        
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE
Each row represents one second of time. The Second column identifies how many seconds have passed as of the beginning of that second. Each worker column shows the step that worker is currently doing (or . if they are idle). The Done column shows completed steps.

Note that the order of the steps has changed; this is because steps now take time to finish and multiple workers can begin multiple steps simultaneously.

In this example, it would take 15 seconds for two workers to complete these steps.

With 5 workers and the 60+ second step durations described above, how long will it take to complete all of the steps?
-}
