import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Char as Char
--import Data.List (reverse, sort)
--import Data.Map.Strict (Map, empty, insert, insertWith, unionWith)
--import qualified Data.Map.Strict as Map
--import Data.Maybe (fromJust)

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input

--process :: String -> Char -> String
--process acc c = 

processInput :: String -> String
processInput = foldl process []
    where process [] char = [char]
          process acc '\n' = acc
          process acc@(x:xs) char
            | (Char.toLower char) == (Char.toLower x) && char /= x = xs
            | otherwise = char : acc
          

main = mainWith myF
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          --myF = show . processInput
          myF = show . length . processInput


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
