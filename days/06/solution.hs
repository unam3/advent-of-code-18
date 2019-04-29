import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Map.Strict as Map

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input


type Point = (Int, Int)

strToPoint :: String -> Point
strToPoint = strToInts . words where
    strToInts [x, y] = (read $ init x, read y)
    strToInts _ = error "strToPoint: takes only string with one space between numbers"


type RootPoint = (Int, Int)

type StepsFromRootPoint = Int

type Direction = String

type PointQuadriple = (Point, RootPoint, StepsFromRootPoint, Direction)

prepareRootPointToPutOnPlane :: Point -> PointQuadriple
prepareRootPointToPutOnPlane point = (point, point, 0, "")


findBoundingPoints :: [Point] -> [Point]
findBoundingPoints pointsList = [minXPoint, maxXPoint, minYPoint, maxYPoint] where
    minXPoint = List.minimumBy (Ord.comparing fst) pointsList
    maxXPoint = List.maximumBy (Ord.comparing fst) pointsList
    minYPoint = List.minimumBy (Ord.comparing snd) pointsList
    maxYPoint = List.maximumBy (Ord.comparing snd) pointsList

boundingPointsExtremeCoords :: [Point] -> [Int]
boundingPointsExtremeCoords [minXPoint, maxXPoint, minYPoint, maxYPoint] = [minX, maxX, minY, maxY] where
    minX = fst minXPoint
    maxX = fst maxXPoint
    minY = snd minYPoint
    maxY = snd maxYPoint
boundingPointsExtremeCoords _ = error "boundingPointsExtremeCoords: takes only list of four Points"

type PointInfo = (RootPoint, StepsFromRootPoint, Direction)

type Plane = Map.Map Point [PointInfo]

makePlane :: [Int] -> Plane
makePlane [minX, maxX, minY, maxY] = Map.fromList planeList where
    planeList = makePlaneList [] minX minY :: [(Point, [PointInfo])]
    makePlaneList acc x y
        | x < maxX = makePlaneList (((x, y), []) : acc) (x + 1) y
        | y < maxY = makePlaneList (((x, y), []) : acc) minX (y + 1)
        | y == maxY && x == maxX = (((x, y), []) : acc)
        | otherwise = acc
makePlane _ = error "makePlane: takes only list of four Ints"


putPointOnPlane :: PointQuadriple -> Plane -> Plane
putPointOnPlane (destination, rootPoint, stepsFromRootPoint, direction) plane =
    Map.alter justPutOrNoop destination plane where
        justPutOrNoop :: Maybe [PointInfo] -> Maybe [PointInfo]
        -- if alter not found such key — noop /w Nothing
        justPutOrNoop Nothing = Nothing
        justPutOrNoop (Just []) = Just [(rootPoint, stepsFromRootPoint, direction)]
        justPutOrNoop (Just allocatedInfo) = if (length allocatedInfo) == 1
            --  we don't care about different stepsFromRootPoint 'cause allocating space process iterates steps and we
            -- only need points with less or equal steps
            && any ((== stepsFromRootPoint) . snd') allocatedInfo
                --  point that was allocated by different root points doesn't counts as area part, therefore we safely
                -- ignore it
                then Nothing
                else Just allocatedInfo
        snd' (_, x, _) = x

plotPoints :: Plane -> [PointQuadriple] -> Plane
plotPoints plane (x : xs) = plotPoints (putPointOnPlane x plane) xs
plotPoints plane [] = plane

-- brute force instead of some type of continuation passing style or some tracking of empty space
hasEmptyLocation :: Plane -> Bool
hasEmptyLocation = any null . Map.elems

-- each step (that /=0) allocates (4 * stepFromRoot) points
fillLocations :: Plane -> [PointQuadriple] -> StepsFromRootPoint -> Plane
fillLocations plane pointQuadplesList steps
    | hasEmptyLocation plane = fillLocations (plotPoints plane pointQuadplesList) adjacentPointQuadriples (steps + 1)
    | otherwise = plane where
        adjacentPointQuadriples = concat $ map makeAdjacentPointQuadriples pointQuadplesList


makeAdjacentPointQuadriples :: PointQuadriple -> [PointQuadriple]
makeAdjacentPointQuadriples ((x,y), rootPoint, steps, direction)
    | steps == 0 = 
        [
            ((x, y + 1), rootPoint, steps + 1, "N"),
            ((x + 1, y), rootPoint, steps + 1, "E"),
            ((x, y - 1), rootPoint, steps + 1, "S"),
            ((x - 1, y), rootPoint, steps + 1, "W")
        ]

    -- clockwork turns; stepsFromRoot will be = 2
    | direction == "N" = 
        [
            ((x, y + 1), rootPoint, steps + 1, "N"),
            ((x + 1, y), rootPoint, steps + 1, "NE")
        ]

    | direction == "E" = 
        [
            ((x + 1, y), rootPoint, steps + 1, "E"),
            ((x, y - 1), rootPoint, steps + 1, "SE")
        ]

    | direction == "S" =
        [
            ((x, y - 1), rootPoint, steps + 1, "S"),
            ((x - 1, y), rootPoint, steps + 1, "SW")
        ]

    | direction == "W" =
        [
            ((x - 1, y), rootPoint, steps + 1, "W"),
            ((x, y + 1), rootPoint, steps + 1, "NW")
        ]

    --  stepsFromRoot will be > 2
    | direction == "NE" =
        [
            ((x + 1, y), rootPoint, steps + 1, "NE")
        ]

    | direction == "SE" =
        [
            ((x, y - 1), rootPoint, steps + 1, "SE")
        ]

    | direction == "SW" =
        [
            ((x - 1, y), rootPoint, steps + 1, "SW")
        ]

    | direction == "NW" =
        [
            ((x, y + 1), rootPoint, steps + 1, "NW")
        ]
makeAdjacentPointQuadriples _ = error "makeAdjacentPointQuadriples: takes only ((x,y), rootPoint, steps, direction)"

fst' :: PointInfo -> RootPoint
fst' (x, _, _) = x

type LocationsCount = Int
type AreaCounter = Map.Map Point LocationsCount

main :: IO ()
main = mainWith myF
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          --solveFirstPuzzlePart = show . boundingPointsExtremeCoords . findBoundingPoints . map strToPoint . lines

          solveFirstPuzzlePart input = show $ getLargestAreaSize $ filterInfiniteAreasPoints $ fillLocations plane pointQuadplesList 0 where
            pointsList = map strToPoint $ lines input
            plane = makePlane $ boundingPointsExtremeCoords boundingPoints
            boundingPoints = findBoundingPoints pointsList
            pointQuadplesList = map prepareRootPointToPutOnPlane pointsList
            
            filterInfiniteAreasPoints :: Plane -> Plane
            filterInfiniteAreasPoints = Map.filter (all (not . ((`elem` boundingPoints) . fst')))

            getLargestAreaSize :: Plane -> LocationsCount
            getLargestAreaSize = snd . (List.maximumBy (Ord.comparing snd)) . Map.toList . foldToAreaCounter where

                foldToAreaCounter :: Plane -> AreaCounter
                foldToAreaCounter = Map.foldl mfoldl Map.empty

                mfoldl :: Map.Map Point LocationsCount -> [PointInfo] -> Map.Map Point LocationsCount
                mfoldl areaCounter [(rootPoint, _, _)] = Map.alter countPoints rootPoint areaCounter
                mfoldl _ _ = error "mfoldl: takes only 'areaCounter [(rootPoint, _, _)]'"

                countPoints :: Maybe LocationsCount -> Maybe LocationsCount
                countPoints Nothing = Just 1
                countPoints (Just counted) = Just (counted + 1)

          -- solveSecondPuzzlePart = show . 
          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n"
            -- ++ "Second part solution is: " ++ secondPartSolution input
          myF = solvePuzzle

-- Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).
-- 
-- Your goal is to find the size of the largest area that isn't infinite. For example, consider the following list of coordinates:
-- 
-- 1, 1
-- 1, 6
-- 8, 3
-- 3, 4
-- 5, 5
-- 8, 9
-- If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:
-- 
-- ..........
-- .A........
-- ..........
-- ........C.
-- ...D......
-- .....E....
-- .B........
-- ..........
-- ..........
-- ........F.
-- This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan distance, each location's closest coordinate can be determined, shown here in lowercase:
-- 
-- aaaaa.cccc
-- aAaaa.cccc
-- aaaddecccc
-- aadddeccCc
-- ..dDdeeccc
-- bb.deEeecc
-- bBb.eeee..
-- bbb.eeefff
-- bbb.eeffff
-- bbb.ffffFf
-- Locations shown as . are equally far from two or more coordinates, and so they don't count as being closest to any.
-- 
-- In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their areas extend forever outside the visible grid. However, the areas of coordinates D and E are finite: D is closest to 9 locations, and E is closest to 17 (both including the coordinate's location itself). Therefore, in this example, the size of the largest area is 17.
-- 
-- What is the size of the largest area that isn't infinite?
