import System.Environment (getArgs)
import qualified Data.List as List
--import qualified Data.Sequence as Seq

type LeftmostPotNumber = Int
type State = String
type SpreadPattern = State

parseInput :: String -> (State, LeftmostPotNumber, [SpreadPattern])
parseInput input = (initialState, initialLeftmostPotNumber, spreadPatternsList) where
    (initialStateStringList, rest) = List.splitAt 1 $ lines input
    initialLeftmostPotNumber = 0
    initialState = last . words $ head initialStateStringList
    spreadPatternsList = fmap (take 5) . filter (List.isSuffixOf "#") $ drop 1 rest

countDots :: State -> Int
countDots state = 5 - length (takeWhile (== '.') state)

makeDotsString :: Int -> State
makeDotsString dotsToAddNumber = List.replicate dotsToAddNumber '.'

-- add dots to the left and to the right if less than 5
fillStateForMatching :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
fillStateForMatching state leftmostPotNumber = let {
        filledState = List.concat [dotsToAddToHead, state, dotsToAddToTail];
        newLeftmostPotNumber = leftmostPotNumber - dotsToAddToHeadNumber;
        dotsToAddToHeadNumber = countDots $ take 5 state;
        dotsToAddToHead = makeDotsString dotsToAddToHeadNumber;
        dotsToAddToTailNumber = countDots . reverse $ drop (length state - 5) state;
        dotsToAddToTail = makeDotsString dotsToAddToTailNumber
    } in dotsToAddToHead `seq` dotsToAddToTail `seq` filledState `seq` newLeftmostPotNumber `seq` (filledState, newLeftmostPotNumber)

type IsFirstCall = Bool

stripExcessiveDots :: State -> State
stripExcessiveDots state@('.':'.':'.':'.':'.':'.':_) = drop (numberOfDots - 5) state where
    numberOfDots = length $ takeWhile (== '.') state
stripExcessiveDots state = state


growGeneration :: (State, LeftmostPotNumber, [SpreadPattern]) -> (State, LeftmostPotNumber, [SpreadPattern])
growGeneration (state, leftmostPotNumber, spreadPatterns) =
    --newState `seq` leftmostPotNumber' `seq` spreadPatterns `seq` (newState, leftmostPotNumber', spreadPatterns) where
    (newState, leftmostPotNumber', spreadPatterns) where
        newState = stripExcessiveDots $ growGeneration' filledState spreadPatterns True
        (filledState, leftmostPotNumber') = fillStateForMatching state leftmostPotNumber

growGeneration' :: State -> [SpreadPattern] -> IsFirstCall -> State
growGeneration' state@(l1:l:_:_:_:_) spreadPatterns isFirstCall =
    let {
        restGrown = growGeneration' (drop 1 state) spreadPatterns False;
        isPrefixOfState spreadPattern = List.isPrefixOf spreadPattern state;
        hasSpreadPatternMatch = any isPrefixOfState spreadPatterns;
        whatToPut = if hasSpreadPatternMatch then '#' else '.'
    } in hasSpreadPatternMatch `seq` restGrown `seq` if isFirstCall
        then l1:l:whatToPut:restGrown
        else whatToPut:restGrown

growGeneration' [_, _, r, r1] _ _ = [r, r1]
growGeneration' state _ _ = state

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

sumPotsWithPlantAfterNGenerations :: String -> Int -> Int
--sumPotsWithPlantAfterNGenerations :: String -> Int -> State
sumPotsWithPlantAfterNGenerations string numberOfGenerations =
    sum . fmap fst . filter ((/= '.') . snd) $ zip [leftmostPotNumber..] state where
    --state where
        (state, leftmostPotNumber, _) = (List.iterate growGeneration $ parseInput string) !! numberOfGenerations
        --(state, leftmostPotNumber, _) = triple `seq` applyNtimes numberOfGenerations growGeneration triple where
        --    triple = parseInput string


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

          solvePuzzle input = "First part solution is: " ++ firstPuzzlePart
              ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
                firstPuzzlePart =
                    show $ sumPotsWithPlantAfterNGenerations input 20
                secondPuzzlePart = --parseInput input
                    show $ sumPotsWithPlantAfterNGenerations input 500
                    --show $ sumPotsWithPlantAfterNGenerations input 50000000000
