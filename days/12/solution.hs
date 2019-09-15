import System.Environment (getArgs)
import qualified Data.List as List
import Data.Foldable (foldr')
--import qualified Data.Sequence as Seq

type LeftmostPotNumber = Int
type State = [Bool]
type SpreadPattern = State

stateToBool :: Char -> Bool
stateToBool '#' = True
stateToBool _ = False

parseInput :: String -> (State, LeftmostPotNumber, [SpreadPattern])
parseInput input = (initialState, initialLeftmostPotNumber, spreadPatternsList) where
    (initialStateStringList, rest) = List.splitAt 1 $ lines input
    initialLeftmostPotNumber = 0
    initialState = fmap stateToBool . last . words $ head initialStateStringList
    spreadPatternsList = fmap ((fmap stateToBool) . take 5) . filter (List.isSuffixOf "#") $ drop 1 rest

countDots :: State -> Int
countDots state = 5 - length (takeWhile (== False) state)

makeDotsString :: Int -> State
makeDotsString dotsToAddNumber = List.replicate dotsToAddNumber False

-- add dots to the left and to the right if less than 5
fillStateForMatching :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
fillStateForMatching state leftmostPotNumber = let {
        --filledState = dotsToAddToHead ++ state ++ dotsToAddToTail;
        filledState = statePart `seq` statePart' `seq` statePart';
        statePart = dotsToAddToHead ++ state;
        statePart' = if null dotsToAddToTail then statePart else statePart ++ dotsToAddToTail;
        newLeftmostPotNumber = leftmostPotNumber - dotsToAddToHeadNumber;
        first5 = take 5 state;
        dotsToAddToHeadNumber = countDots first5;
        dotsToAddToHead = makeDotsString dotsToAddToHeadNumber;
        last5 = drop (length state - 5) state;
        dotsToAddToTailNumber = countDots $ reverse last5;
        dotsToAddToTail = makeDotsString dotsToAddToTailNumber
    } in first5 `seq` dotsToAddToHeadNumber `seq` dotsToAddToHead `seq` last5 `seq` dotsToAddToTailNumber `seq` dotsToAddToTail `seq` state `seq` filledState `seq` newLeftmostPotNumber `seq` (filledState, newLeftmostPotNumber)

type IsFirstCall = Bool

stripExcessiveDots :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
stripExcessiveDots state@(False:False:False:False:False:False:_) leftmostPotNumber = newState `seq` newLeftmostPotNumber `seq` (newState, newLeftmostPotNumber) where
    newState = drop delta state
    newLeftmostPotNumber = leftmostPotNumber + delta
    delta = numberOfDots - 5
    numberOfDots = length $ takeWhile (== False) state
stripExcessiveDots state leftmostPotNumber = (state, leftmostPotNumber)


growGeneration :: (State, LeftmostPotNumber, [SpreadPattern]) -> (State, LeftmostPotNumber, [SpreadPattern])
growGeneration (state, leftmostPotNumber, spreadPatterns) =
    filledState `seq` leftmostPotNumber' `seq` (newState, leftmostPotNumber'', spreadPatterns) where
        (newState, leftmostPotNumber'') = stripExcessiveDots (growGeneration' filledState spreadPatterns True) leftmostPotNumber'
        (filledState, leftmostPotNumber') = fillStateForMatching state leftmostPotNumber

growGeneration' :: State -> [SpreadPattern] -> IsFirstCall -> State
growGeneration' state@(l1:l:_:_:_:_) spreadPatterns isFirstCall =
    let {
        restGrown = growGeneration' tail' spreadPatterns False;
        tail' = drop 1 state;
        isPrefixOfState spreadPattern = List.isPrefixOf spreadPattern state;
        hasSpreadPatternMatch = any isPrefixOfState spreadPatterns;
        whatToPut = if hasSpreadPatternMatch then True else False
    } in whatToPut `seq` isPrefixOfState `seq` hasSpreadPatternMatch `seq` tail `seq` restGrown `seq` if isFirstCall
        then l1:l:whatToPut:restGrown
        else whatToPut:restGrown

growGeneration' [_, _, r, r1] _ _ = [r, r1]
growGeneration' state _ _ = state

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

--sumPotsWithPlantAfterNGenerations :: String -> Int -> Int
sumPotsWithPlantAfterNGenerations :: String -> Int -> State
--sumPotsWithPlantAfterNGenerations :: String -> Int -> (State, LeftmostPotNumber)
sumPotsWithPlantAfterNGenerations string numberOfGenerations =
    --sum . fmap fst . filter ((/= False) . snd) $ zip [leftmostPotNumber..] state where
    --(state, leftmostPotNumber) where
    state where
        --(state, leftmostPotNumber, _) = (List.iterate growGeneration $ parseInput string) !! numberOfGenerations
        (state, leftmostPotNumber, _) = triple `seq` applyNtimes numberOfGenerations growGeneration triple
        triple = parseInput string


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

          solvePuzzle input = 
              --"First part solution is: " ++ firstPuzzlePart
              -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
              "Second part solution is: " ++ secondPuzzlePart where
                --firstPuzzlePart =
                --    show $ sumPotsWithPlantAfterNGenerations input 20
                secondPuzzlePart = --parseInput input
                    --show $ sumPotsWithPlantAfterNGenerations input 50000
                    show $ sumPotsWithPlantAfterNGenerations input 50000000000
