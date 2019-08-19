import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
--import qualified Data.Sequence as Seq


type Message = String
type Coordinate = Int
type PointOfLight = (Coordinate, Coordinate, Coordinate, Coordinate)

parseInput :: String -> [PointOfLight]
parseInput = map parsePositionAndVelocity . lines

garbageLength :: Int
--garbageLength = length "velocity=<"
garbageLength = length "position=<"

parsePositionAndVelocity :: String -> PointOfLight
-- "position=< 54771, -54515> velocity=<-5,  5>"
-- "position=<-10810,  43870> velocity=< 1, -4>"
parsePositionAndVelocity = makeTuple . map read . map init . removeThird . words . drop garbageLength

-- ["54771,","-54515>","velocity=<-5,","5>"]
-- ["-10810,","43870>","velocity=<","1,","-4>"]
removeThird :: [String] -> [String]
removeThird [spx, spy, _, svx, svy] = [spx, spy, svx, svy]
removeThird [spx, spy, sVelocityX, svy] = [spx, spy, drop garbageLength sVelocityX, svy]
removeThird _ = []

makeTuple :: [Coordinate] -> PointOfLight
makeTuple [px, py, vx, vy] = (px, py, vx, vy)
makeTuple _ = (-1, -1, -1, -1)

takeStep :: [PointOfLight] -> [PointOfLight]
takeStep = map takeStep' where
    takeStep' :: PointOfLight -> PointOfLight
    takeStep' (px, py, vx, vy) = (px + vx, py + vy, vx, vy)

pointsAreOnVerticalStraightLine :: PointOfLight -> PointOfLight -> Bool
pointsAreOnVerticalStraightLine (x, _, _, _) (x1, _, _, _) = x == x1

pointsHaveDifferentY :: PointOfLight -> PointOfLight -> Bool
pointsHaveDifferentY (_, y, _, _) (_, y1, _, _) = y /= y1

type Columns = Map.Map Coordinate [Coordinate]

getPositionX :: PointOfLight -> Coordinate
getPositionX (x, _, _, _) = x

insertWrapper :: [Coordinate] -> [Coordinate] -> [Coordinate]
insertWrapper [coordinate] ys = List.insert coordinate ys
insertWrapper _ _ = []

collectPointsByX :: Columns -> PointOfLight -> Columns
collectPointsByX columns (x, y, _, _) = case Map.lookup x columns of
    Nothing -> Map.insert x [y] columns
    Just ys -> case List.elem y ys of
        True -> columns
        False -> Map.insertWith insertWrapper x [y] columns

isListHasEnoughLength :: (Coordinate, [Coordinate]) -> Bool
isListHasEnoughLength (_, _:_:_:_:_:_:_) = True
isListHasEnoughLength (_, _) = False

isNumberListSequential :: (Coordinate, [Coordinate]) -> Bool
isNumberListSequential (_, yList@(y:_)) = List.and $ List.zipWith (==) yList [y..]
isNumberListSequential _ = False

findStraightVerticalLine :: [PointOfLight] -> [(Coordinate, [Coordinate])]
findStraightVerticalLine = list where
        list = List.filter isNumberListSequential . List.filter isListHasEnoughLength . Map.toList . List.foldl' collectPointsByX Map.empty . List.sortOn getPositionX

extractTransformPositionPoints :: PointOfLight -> (Coordinate, Coordinate)
extractTransformPositionPoints (x, y, _, _) = (x, -y)

findStateWithStraightVerticalLines :: [PointOfLight] -> [(Coordinate, Coordinate)]
findStateWithStraightVerticalLines pointsOfLight = let {
        nextPointsOfLight = takeStep pointsOfLight;
        straightLinesList = findStraightVerticalLine nextPointsOfLight;
    } in case null straightLinesList of
        True -> findStateWithStraightVerticalLines nextPointsOfLight
        False -> map extractTransformPositionPoints nextPointsOfLight


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

          --solveFirstPuzzlePart = show . findStraightVerticalLine . takeStep  . takeStep  . takeStep . parseInput
          solveFirstPuzzlePart = show . findStateWithStraightVerticalLines . parseInput
          --solveSecondPuzzlePart = show . getWinningScore 100 . parseInput

          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
               -- ++ "Second part solution is: " ++ solveSecondPuzzlePart input
