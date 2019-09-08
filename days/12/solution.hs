import System.Environment (getArgs)
import qualified Data.List as List
--import qualified Data.Sequence as Seq

--After 20 generations, what is the sum of the numbers of all pots which contain a plant?

--newtype State = State String
type State = String
type SpreadToNearbyPotsNote = (String, Char)
--type PotContainPlant = '#'
--type PotNotContainPlant = '.'

parseSpreadToNearbyPotsNotes :: String -> SpreadToNearbyPotsNote
parseSpreadToNearbyPotsNotes string = (pattern, result) where
    (pattern, rest) = splitAt 5 string
    result = last rest

parseInput :: String -> (State, [SpreadToNearbyPotsNote])
parseInput input = (initialState, spreadToNearbyPotsNotesList) where
    (initialStateStringList, rest) = List.splitAt 1 $ lines input
    initialState = last . words $ head initialStateStringList
    spreadToNearbyPotsNotesList = fmap parseSpreadToNearbyPotsNotes $ drop 1 rest


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
                   show (parseInput input)
                --secondPuzzlePart =
                --    show .
