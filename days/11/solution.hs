import System.Environment (getArgs)
import qualified Data.Sequence as Seq


parseInput :: String -> Int
parseInput = read

type Coordinate = Int
type GridSerialNumber = Int
type CellPowerLevel = Int
type FuelCellsGrid = Seq.Seq (Coordinate, Coordinate)
type FuelCells3x3Grid = Seq.Seq (Coordinate, Coordinate, CellPowerLevel)

makeFuelCells3x3Grid' :: GridSerialNumber -> FuelCells3x3Grid -> Coordinate -> Coordinate -> FuelCells3x3Grid
makeFuelCells3x3Grid' gridSerialNumber fuelCellsGrid x y = case x == 299 of
    True -> fuelCellsGrid
    False -> case y == 298 of
        True  -> makeFuelCells3x3Grid' gridSerialNumber newFuelCellsGrid (x+1) 1
        False -> makeFuelCells3x3Grid' gridSerialNumber newFuelCellsGrid x (y+1)
    where
        newFuelCellsGrid = fuelCellsGrid Seq.|> (x, y, totalPower)
        getCellPowerLevel' = getCellPowerLevel gridSerialNumber
        totalPower = sum [
                getCellPowerLevel' (x, y),      getCellPowerLevel' (x + 1, y),      getCellPowerLevel' (x + 2, y),
                getCellPowerLevel' (x, y + 1),  getCellPowerLevel' (x + 1, y + 1),  getCellPowerLevel' (x + 2, y + 1),
                getCellPowerLevel' (x, y + 2),  getCellPowerLevel' (x + 1, y + 2),  getCellPowerLevel' (x + 2, y + 2)
            ]

makeFuelCells3x3Grid :: GridSerialNumber -> FuelCells3x3Grid
makeFuelCells3x3Grid gridSerialNumber = makeFuelCells3x3Grid' gridSerialNumber Seq.empty 1 1

getHundredsDigit :: Int -> Int
getHundredsDigit number = fst $ divMod (snd $ divMod number 1000) 100

getCellPowerLevel :: GridSerialNumber -> (Coordinate, Coordinate) -> CellPowerLevel
getCellPowerLevel gridSerialNumber (x, y) = hundredsDigit - 5 where
    rackID = x + 10
    hundredsDigit = getHundredsDigit ((rackID * y + gridSerialNumber) * rackID)
    
getCoordsAndCellPowerLevel :: GridSerialNumber -> (Coordinate, Coordinate) -> ((Coordinate, Coordinate), CellPowerLevel)
getCoordsAndCellPowerLevel gridSerialNumber coords = (coords, getCellPowerLevel gridSerialNumber coords)

getPower :: (Coordinate, Coordinate, CellPowerLevel) -> CellPowerLevel 
getPower (_, _, cellsPowerLevel) = cellsPowerLevel

sortByPower :: (Coordinate, Coordinate, CellPowerLevel) -> (Coordinate, Coordinate, CellPowerLevel) -> Ordering
sortByPower a b = case compare (getPower a) (getPower b) of
    LT -> GT
    GT -> LT
    _  -> EQ

getLargestTotalPowerCell :: FuelCells3x3Grid -> (Coordinate, Coordinate, CellPowerLevel)
--getLargestTotalPowerCell :: FuelCells3x3Grid -> FuelCells3x3Grid
--getLargestTotalPowerCell = Seq.take 3 . Seq.sortBy sortByPower
getLargestTotalPowerCell fuelCells3x3Grid = case Seq.lookup 0 $ Seq.sortBy sortByPower fuelCells3x3Grid of
    Just triple -> triple
    Nothing -> (0, 0, 0)

getXYCoords :: (Coordinate, Coordinate, CellPowerLevel) -> String
getXYCoords (x, y, _) = show x ++ "," ++ show y

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
               --  ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
                  --firstPuzzlePart = show . makeFuelCells3x3Grid $ parseInput input
                  firstPuzzlePart = show . getXYCoords . getLargestTotalPowerCell . makeFuelCells3x3Grid $ parseInput input
                  --secondPuzzlePart = show secondsToWaitMessageAppearance

