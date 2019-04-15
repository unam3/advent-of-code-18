import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Ord as Ord

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input

reactPolymer :: String -> String
reactPolymer = foldl process []
    where process [] char = [char]
          process acc '\n' = acc
          process acc@(x:xs) char
            | (Char.toLower char) == (Char.toLower x) && char /= x = xs
            | otherwise = char : acc
          
uniqLetters :: String -> String
uniqLetters = foldl process []
    where process [] char = [char]
          process acc '\n' = acc
          process acc char
            | List.any (== char) acc = acc
            | otherwise = char : acc

reduceAndReactPolymer :: String -> [(Char, Int)]
reduceAndReactPolymer inputString = map reduceString uniqLowercaseLetters
    where uniqLowercaseLetters = uniqLetters $ (map Char.toLower) inputString
          reduceString letter = (letter, length $ reactPolymer $ filterInputString letter)
          filterInputString letter = filter (\x -> Char.toLower x /= letter) inputString

findShortestPolymerLength :: [(Char, Int)] -> Int
findShortestPolymerLength = snd . List.minimumBy (Ord.comparing snd)

main = mainWith solvePuzzle
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          solveFirstPuzzlePart = show . length . reactPolymer
          solveSecondPuzzlePart = show . findShortestPolymerLength . reduceAndReactPolymer
          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n" ++
            "Second part solution is: " ++ solveSecondPuzzlePart input


--The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization.

--In aA, a and A react, leaving nothing behind.
--In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
--In abAB, no two adjacent units are of the same type, and so nothing happens.
--In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.

--Now, consider a larger example, dabAcCaCBAcCcaDA:

--dabAcCaCBAcCcaDA  The first 'cC' is removed.
--dabAaCBAcCcaDA    This creates 'Aa', which is removed.
--dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
--dabCBAcaDA        No further actions can be taken.
--After all possible reactions, the resulting polymer contains 10 units.

--How many units remain after fully reacting the polymer you scanned?

--Part Two
--
--Your goal is to figure out which unit type is causing the most problems, remove all instances of it (regardless of polarity), fully react the remaining polymer, and measure its length.
--
--For example, again using the polymer dabAcCaCBAcCcaDA from above:
--
--Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer produces dbCBcD, which has length 6.
--Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer produces daCAcaDA, which has length 8.
--Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer produces daDA, which has length 4.
--Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer produces abCBAc, which has length 6.
--In this example, removing all C/c units was best, producing the answer 4.
--
--What is the length of the shortest polymer you can produce by removing all units of exactly one type and fully reacting the result?
