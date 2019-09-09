import System.Environment (getArgs)
import qualified Data.List as List
--import qualified Data.Sequence as Seq

type LeftmostPotNumber = Int
type State = String
type SpreadPattern = (String, Char)

parseSpreadPatterns :: String -> SpreadPattern
parseSpreadPatterns string = (pattern, result) where
    (pattern, rest) = splitAt 5 string
    result = last rest

parseInput :: String -> (State, LeftmostPotNumber, [SpreadPattern])
parseInput input = (initialState, initialLeftmostPotNumber, spreadPatternsList) where
    (initialStateStringList, rest) = List.splitAt 1 $ lines input
    initialLeftmostPotNumber = 0
    initialState = last . words $ head initialStateStringList
    spreadPatternsList = fmap parseSpreadPatterns . filter (List.isSuffixOf "#") $ drop 1 rest

countDots :: State -> Int
countDots state = 5 - (length $ takeWhile (== '.') state)

makeDotsString :: Int -> State
makeDotsString dotsToAddNumber = take dotsToAddNumber $ repeat '.'

-- add dots to the left and to the right if less than 5
fillStateForMatching :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
fillStateForMatching state leftmostPotNumber = (filledState, newLeftmostPotNumber) where
    filledState = dotsToAddToHead ++ state ++ dotsToAddToTail
    newLeftmostPotNumber = leftmostPotNumber - dotsToAddToHeadNumber
    dotsToAddToHeadNumber = countDots $ take 5 state
    dotsToAddToHead = makeDotsString dotsToAddToHeadNumber
    dotsToAddToTailNumber = countDots . reverse $ drop (length state - 5) state
    dotsToAddToTail = makeDotsString dotsToAddToTailNumber

type IsFirstCall = Bool

growGeneration :: (State, LeftmostPotNumber, [SpreadPattern]) -> (State, LeftmostPotNumber, [SpreadPattern])
growGeneration (state, leftmostPotNumber, spreadPatterns) =
    (newState, leftmostPotNumber', spreadPatterns) where
        newState = growGeneration' filledState spreadPatterns True
        (filledState, leftmostPotNumber') = fillStateForMatching state leftmostPotNumber

growGeneration' :: State -> [SpreadPattern] -> IsFirstCall -> State
growGeneration' state@(l1:l:_:_:_:_) spreadPatterns isFirstCall =
    let {
        restGrown = growGeneration' (drop 1 state) spreadPatterns False;
        hasSpreadPatternMatch = any ((flip List.isPrefixOf $ state) . fst) spreadPatterns;
        whatToPut = if hasSpreadPatternMatch then '#' else '.'
    } in (if isFirstCall then l1:l:whatToPut:[] else [whatToPut]) ++ restGrown

growGeneration' (_:_:r:r1:[]) _ _ = r:r1:[]
growGeneration' state _ _ = state



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

          solvePuzzle input = "First part solution is: " ++ firstPuzzlePart where
              -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
                firstPuzzlePart =
                    --show $ parseInput input
                    show . sum . fmap fst . filter ((/= '.') . snd) $ zip [leftmostPotNumber..] state where
                        (state, leftmostPotNumber, _) = (List.iterate growGeneration $ parseInput input) !! 20
                --secondPuzzlePart =
                --    show .
