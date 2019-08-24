import System.Environment (getArgs)
--import qualified Data.Map.Strict as Map
--import qualified Data.List as List
import qualified Data.Sequence as Seq


type Message = String

parseInput :: String -> Int
parseInput = read

type Coordinate = Int
type GridSerialNumber = Int
type CellPowerLevel = Int
type FuelCellsGrid = Seq.Seq (Coordinate, Coordinate)

makeFuelCellsGrid' :: FuelCellsGrid -> Coordinate -> Coordinate -> FuelCellsGrid
makeFuelCellsGrid' fuelCellsGrid x y = case x == 301 of
    True -> fuelCellsGrid
    False -> case y == 300 of
        True -> makeFuelCellsGrid' newFuelCellsGrid (x+1) 1
        False -> makeFuelCellsGrid' newFuelCellsGrid x (y+1)
    where newFuelCellsGrid = fuelCellsGrid Seq.|> (x, y)

makeFuelCellsGrid :: FuelCellsGrid
makeFuelCellsGrid = makeFuelCellsGrid' Seq.empty 1 1

getHundredsDigit :: Int -> Int
getHundredsDigit number = fst $ divMod (snd $ divMod number 1000) 100

getCellPowerLevel :: GridSerialNumber -> (Coordinate, Coordinate) -> CellPowerLevel
getCellPowerLevel gridSerialNumber (x, y) = hundredsDigit - 5 where
    rackID = x + 10
    hundredsDigit = getHundredsDigit (rackID * y + gridSerialNumber) * rackID
    
getCoordsAndCellPowerLevel :: GridSerialNumber -> (Coordinate, Coordinate) -> ((Coordinate, Coordinate), CellPowerLevel)
getCoordsAndCellPowerLevel gridSerialNumber coords = (coords, getCellPowerLevel gridSerialNumber coords)

-- из FCG делаем FCG3x3 по координатам левой верхней точки квадрата
makeFuelCellsGrid3x3' :: FuelCellsGrid -> FuelCellsGrid -> Coordinate -> Coordinate -> Seq.Seq (Coordinate, Coordinate, CellPowerLevel)
makeFuelCellsGrid3x3' fuelCellsGrid fcg3x3 x y = case x == 299 of
    True -> fcg3x3
    False -> case y == 298 of
        --True -> makeFuelCellsGrid3x3 newFuelCellsGrid (x+1) 1
        True -> undefined
        --False -> makeFuelCellsGrid3x3 newFuelCellsGrid x (y+1)
        False -> totalPower
    where
        newFuelCellsGrid = fcg3x3 Seq.|> (x, y, totalPower)
        multiplierPart = (x - 1) * 300
        topLeft = multiplierPart + y - 1
        totalPower = Seq.lookup topLeft

makeFuelCellsGrid3x3 :: FuelCellsGrid -> FuelCellsGrid
makeFuelCellsGrid3x3 fuelCellsGrid = makeFuelCellsGrid' fuelCellsGrid Seq.empty 1 1

getLargestTotalPowerCell = getCellPowerLevel


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
                  --firstPuzzlePart = show . getLargestTotalPowerCell $ parseInput input
                  --firstPuzzlePart = show $ fmap (getCoordsAndCellPowerLevel $ parseInput input) makeFuelCellsGrid
                  firstPuzzlePart = show $ fmap (getCoordsAndCellPowerLevel $ parseInput input) makeFuelCellsGrid
                  --secondPuzzlePart = show secondsToWaitMessageAppearance

