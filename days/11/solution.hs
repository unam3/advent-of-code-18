import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Sequence as Seq


parseInput :: String -> Int
parseInput = read

type Coordinate = Int
type GridSerialNumber = Int
type CellPowerLevel = Int
type FuelCellsXofXGrid = Seq.Seq (Coordinate, Coordinate, CellPowerLevel)
type KSquare = [(Coordinate, Coordinate)]

makeFuelCellsXofXGrid' :: KSquare -> SquareSideSize -> GridSerialNumber -> FuelCellsXofXGrid -> Coordinate -> Coordinate -> FuelCellsXofXGrid
makeFuelCellsXofXGrid' kSquare squareSideSize gridSerialNumber fuelCellsGrid x y = case x == 300 - squareSideSize + 2 of
    True -> fuelCellsGrid
    False -> case y == 300 - squareSideSize + 1 of
        True  -> makeFuelCellsXofXGrid'' (x+1) 1
        False -> makeFuelCellsXofXGrid'' x (y+1)
        where
            makeFuelCellsXofXGrid'' = makeFuelCellsXofXGrid' kSquare squareSideSize gridSerialNumber newFuelCellsGrid
            newFuelCellsGrid = fuelCellsGrid Seq.|> (x, y, totalPower)
            getCellPowerLevel' = getCellPowerLevel gridSerialNumber
            totalPower = sum $ fmap (getCellPowerLevel' . tieToXY) kSquare
            tieToXY = (\(kx, ky) -> (x + kx, y + ky))

makeFuelCellsXofXGrid :: GridSerialNumber -> SquareSideSize -> FuelCellsXofXGrid
makeFuelCellsXofXGrid gridSerialNumber squareSideSize =
    makeFuelCellsXofXGrid' kSquare squareSideSize gridSerialNumber Seq.empty 1 1 where
        kSquare = List.concatMap (\kx -> fmap ((\ky -> (kx, ky))) side) side :: KSquare where
            side = [0..squareSideSize-1] :: [Coordinate]

getHundredsDigit :: Int -> Int
getHundredsDigit number = div (mod number 1000) 100

getCellPowerLevel :: GridSerialNumber -> (Coordinate, Coordinate) -> CellPowerLevel
getCellPowerLevel gridSerialNumber (x, y) = rackID `seq` hundredsDigit `seq` hundredsDigit - 5 where
    rackID = x + 10
    hundredsDigit = getHundredsDigit ((rackID * y + gridSerialNumber) * rackID)

getPower :: (Coordinate, Coordinate, CellPowerLevel) -> CellPowerLevel 
getPower (_, _, cellsPowerLevel) = cellsPowerLevel

sortByPower :: (Coordinate, Coordinate, CellPowerLevel) -> (Coordinate, Coordinate, CellPowerLevel) -> Ordering
sortByPower a b = case compare (getPower a) (getPower b) of
    LT -> GT
    GT -> LT
    _  -> EQ

sortByPower' :: (SquareSideSize, (Coordinate, Coordinate, CellPowerLevel)) ->
    (SquareSideSize, (Coordinate, Coordinate, CellPowerLevel)) -> Ordering
sortByPower' (_, a@(_, _, _)) (_, b@(_, _, _)) = sortByPower a b

getLargestTotalPowerCell :: FuelCellsXofXGrid -> (Coordinate, Coordinate, CellPowerLevel)
getLargestTotalPowerCell fuelCellsXofXGrid = case Seq.lookup 0 $ Seq.sortBy sortByPower fuelCellsXofXGrid of
    Just triple -> triple
    Nothing -> (0, 0, 0)

getXYCoords :: (Coordinate, Coordinate, CellPowerLevel) -> String
getXYCoords (x, y, _) = show x ++ "," ++ show y

type SquareSideSize = Int
type TotalPowerSquareIdentifier = String

arrangeSecondSolutionString :: (SquareSideSize, (Coordinate, Coordinate, CellPowerLevel)) -> TotalPowerSquareIdentifier
arrangeSecondSolutionString (squareSideSize, (x, y, _)) = List.intercalate "," $ map show [x, y, squareSideSize]

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
                   show . getXYCoords . getLargestTotalPowerCell $ makeFuelCellsXofXGrid (parseInput input) 3
                secondPuzzlePart =
                    show .
                        arrangeSecondSolutionString .
                        head . List.sortBy sortByPower' $
                        zip squareSideSizes $
                        fmap (getLargestTotalPowerCell . makeFuelCellsXofXGrid gridSerialNumber) squareSideSizes
                gridSerialNumber = parseInput input
                squareSideSizes = [1..300]
