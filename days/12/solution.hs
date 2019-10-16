{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.List as List

type LeftmostPotNumber = Int
type State = [Bool]
type SpreadPattern = State

stateToBool :: Char -> Bool
stateToBool '#' = True
stateToBool _ = False

--s = "initial state: ##.#..#.#..#.####.#########.#...#.#.#......##.#.#...##.....#...#...#.##.#...##...#.####.##..#.#..#.\n\n..#.. => .\n..#.# => .\n#.#.. => .\n.#..# => .\n#.... => .\n....# => .\n.#.#. => #\n#.### => .\n####. => .\n..... => .\n.#... => #\n##### => #\n.#### => .\n#..#. => #\n#...# => #\n.###. => .\n###.# => #\n...## => #\n#.##. => #\n.#.## => #\n##.#. => #\n...#. => .\n..### => #\n###.. => #\n##... => .\n..##. => .\n.##.# => .\n##.## => .\n.##.. => .\n##..# => #\n#.#.# => .\n#..## => #\n"

parseInput :: String -> (State, LeftmostPotNumber, [SpreadPattern])
parseInput input =
    (initialState, initialLeftmostPotNumber, spreadPatternsList) where
    (initialStateStringList, rest) = List.splitAt 1 $ lines input
    !initialState = fmap stateToBool . last . words $ head initialStateStringList
    !initialLeftmostPotNumber = 0
    !spreadPatternsList = fmap ((fmap stateToBool) . take 5) . filter (List.isSuffixOf "#") $ drop 1 rest

countDots :: State -> Int
countDots state = 5 - length (takeWhile (== False) state)

makeDotsString :: Int -> State
makeDotsString dotsToAddNumber = List.replicate dotsToAddNumber False

-- add dots to the left and to the right if less than 5
fillStateForMatching :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
fillStateForMatching state leftmostPotNumber = let {
        filledState = statePart List.++ dotsToAddToTail;
        !statePart = dotsToAddToHead List.++ state;
        newLeftmostPotNumber = leftmostPotNumber - dotsToAddToHeadNumber;
        first5 = take 5 state;
        dotsToAddToHeadNumber = countDots first5;
        !dotsToAddToHead = makeDotsString dotsToAddToHeadNumber;
        last5 = drop (length state - 5) state;
        dotsToAddToTailNumber = countDots $ reverse last5;
        !dotsToAddToTail = makeDotsString dotsToAddToTailNumber
    } in (filledState, newLeftmostPotNumber)

type IsFirstCall = Bool

stripExcessiveDots :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
stripExcessiveDots state@(False:False:False:False:False:False:_) leftmostPotNumber =
        (newState, newLeftmostPotNumber) where
    !newState = drop delta state
    !newLeftmostPotNumber = leftmostPotNumber + delta
    !delta = numberOfDots - 5
    !numberOfDots = length $ takeWhile (== False) state

stripExcessiveDots state leftmostPotNumber = (state, leftmostPotNumber)


growGeneration :: (State, LeftmostPotNumber, [SpreadPattern]) -> (State, LeftmostPotNumber, [SpreadPattern])
growGeneration (state, leftmostPotNumber, spreadPatterns) =
    newState `seq` leftmostPotNumber'' `seq` (newState, leftmostPotNumber'', spreadPatterns) where

        (newState, leftmostPotNumber'') = stripExcessiveDots newGeneration leftmostPotNumber'
        
        !newGeneration = filledState `seq` growGeneration' filledState spreadPatterns True

        (filledState, leftmostPotNumber') = fillStateForMatching state leftmostPotNumber

growGeneration' :: State -> [SpreadPattern] -> IsFirstCall -> State
growGeneration' state@(l1:l:_:_:_:_) spreadPatterns isFirstCall =
    let {
        !restGrown = growGeneration' tail' spreadPatterns False;
        !tail' = drop 1 state;
        isPrefixOfState spreadPattern = List.isPrefixOf spreadPattern state;
        !hasSpreadPatternMatch = any isPrefixOfState spreadPatterns;
        !whatToPut = if hasSpreadPatternMatch then True else False
    } in if isFirstCall
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
        (state, _, _) = applyNtimes numberOfGenerations growGeneration triple
        triple = parseInput $ string


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
              "Second part solution is: " List.++ secondPuzzlePart where
                --firstPuzzlePart =
                --    show $ sumPotsWithPlantAfterNGenerations input 20
                secondPuzzlePart = --show $ parseInput input
                    show $ sumPotsWithPlantAfterNGenerations input 50000
                    --show $ sumPotsWithPlantAfterNGenerations input 50000000000
