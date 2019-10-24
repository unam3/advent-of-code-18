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
    initialState = fmap stateToBool . last . words $ head initialStateStringList
    initialLeftmostPotNumber = 0
    !spreadPatternsList = fmap ((fmap stateToBool) . take 5) . filter (List.isSuffixOf "#") $ drop 1 rest

countDots :: State -> Int
countDots state = 5 - length (takeWhile (== False) state)

makeDotsString :: Int -> State
makeDotsString dotsToAddNumber = List.replicate dotsToAddNumber False

-- add dots to the left and to the right if less than 5
fillStateForMatching :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
fillStateForMatching state leftmostPotNumber = let {
        first5 = take 5 state;
        dotsToAddToHeadNumber = countDots first5;
        dotsToAddToHead = makeDotsString dotsToAddToHeadNumber;
        filledState = dotsToAddToHead List.++ state;
        newLeftmostPotNumber = leftmostPotNumber - dotsToAddToHeadNumber;
    } in (filledState, newLeftmostPotNumber)

type IsFirstCall = Bool

stripExcessiveDots :: State -> LeftmostPotNumber -> (State, LeftmostPotNumber)
stripExcessiveDots state@(False:False:False:False:False:False:_) leftmostPotNumber =
        (newState, newLeftmostPotNumber) where
    !numberOfDots = length $ takeWhile (== False) state
    !delta = numberOfDots - 5
    !newState = drop delta state
    !newLeftmostPotNumber = leftmostPotNumber + delta

stripExcessiveDots state leftmostPotNumber = (state, leftmostPotNumber)


growGeneration :: (State, LeftmostPotNumber, [SpreadPattern]) -> (State, LeftmostPotNumber, [SpreadPattern])
growGeneration (state, leftmostPotNumber, spreadPatterns) =
    newState `seq` leftmostPotNumber'' `seq` (newState, leftmostPotNumber'', spreadPatterns) where
        (newState, leftmostPotNumber'') = stripExcessiveDots newGeneration leftmostPotNumber'
        newGeneration = filledState `seq` growGeneration' filledState spreadPatterns True
        (filledState, leftmostPotNumber') = fillStateForMatching state leftmostPotNumber

growGeneration' :: State -> [SpreadPattern] -> IsFirstCall -> State
growGeneration' state@(l1:l:_:_:_:_) spreadPatterns isFirstCall =
    let {
        !restGrown = growGeneration' tail' spreadPatterns False;
        isPrefixOfState spreadPattern = List.isPrefixOf spreadPattern state;
        !tail' = drop 1 state;
        !whatToPut = any isPrefixOfState spreadPatterns;
    } in if isFirstCall
        then l1:l:whatToPut:restGrown
        else whatToPut:restGrown

growGeneration' state@[_, _, _, True] spreadPatterns isFirstCall =
    growGeneration' filledState spreadPatterns isFirstCall where
        filledState = state ++ [False, False, False, False]

growGeneration' state@[_, _, True, False] spreadPatterns isFirstCall =
    growGeneration' filledState spreadPatterns isFirstCall where
        filledState = state ++ [False, False, False]

growGeneration' state@[_, True, False, False] spreadPatterns isFirstCall =
    growGeneration' filledState spreadPatterns isFirstCall where
        filledState = state ++ [False, False]

growGeneration' state@[True, False, False] spreadPatterns isFirstCall =
    growGeneration' filledState spreadPatterns isFirstCall where
        filledState = state ++ [False]

growGeneration' [_, _, r, r1] _ _ = [r, r1]

growGeneration' state _ _ = state

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f !x = f (applyNtimes (n-1) f x)

--sumPotsWithPlantAfterNGenerations :: String -> Int -> Int
sumPotsWithPlantAfterNGenerations :: String -> Int -> State
--sumPotsWithPlantAfterNGenerations :: String -> Int -> (State, LeftmostPotNumber)
sumPotsWithPlantAfterNGenerations string numberOfGenerations =
    --sum . fmap fst . filter ((/= False) . snd) $ zip [leftmostPotNumber..] state where
    --(state, leftmostPotNumber) where
    state where
        --(state, leftmostPotNumber, _) = (List.iterate growGeneration $ parseInput string) !! numberOfGenerations
        --(state, leftmostPotNumber, _) = applyNtimes numberOfGenerations growGeneration triple
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
                --secondPuzzlePart =
                --    show $ sumPotsWithPlantAfterNGenerations input 20
                secondPuzzlePart = --show $ parseInput input
                    --show $ sumPotsWithPlantAfterNGenerations input 3000
                    show $ sumPotsWithPlantAfterNGenerations input 50000
                    --show $ sumPotsWithPlantAfterNGenerations input 50000000000
