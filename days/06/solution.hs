import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Map.Strict as Map

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input


type Point = [Int]

strToPoint :: String -> Point
strToPoint = strToInts . words where
    strToInts [x, y] = [read $ init x, read y]


type RootPoint = [Int]

type StepsFromRootPoint = Int

type Direction = String

type PointQuadriple = (Point, RootPoint, StepsFromRootPoint, Direction)

prepareRootPointToPutOnPlane :: Point -> PointQuadriple
prepareRootPointToPutOnPlane point = (point, point, 0, "")


findBoundingPoints :: [Point] -> [Point]
findBoundingPoints pointsList = [minX, maxX, minY, maxY] where
    minX = List.minimumBy (Ord.comparing head) pointsList
    maxX = List.maximumBy (Ord.comparing head) pointsList
    minY = List.minimumBy (Ord.comparing (!! 1)) pointsList
    maxY = List.maximumBy (Ord.comparing (!! 1)) pointsList

boundingPointsExtremeCoords :: [Point] -> [Int]
boundingPointsExtremeCoords [minXPoint, maxXPoint, minYPoint, maxYPoint] = [minX, maxX, minY, maxY] where
    minX = head minXPoint
    maxX = head maxXPoint
    minY = last minYPoint
    maxY = last maxYPoint


type Plane = Map.Map Point [(RootPoint, StepsFromRootPoint, Direction)]

makePlane :: [Int] -> Plane
makePlane [minX, maxX, minY, maxY] = Map.fromList planeList where
    planeList = makePlaneList [] minX minY :: [(Point, [(RootPoint, StepsFromRootPoint, Direction)])]
    makePlaneList acc x y
        | x < maxX = makePlaneList (([x, y], []) : acc) (x + 1) y
        | y < maxY = makePlaneList (([x, y], []) : acc) minX (y + 1)
        | y == maxY && x == maxX = (([x, y], []) : acc)
        | otherwise = acc

putPointOnPlane :: PointQuadriple -> Plane -> Plane
putPointOnPlane (destination, rootPoint, stepsFromRootPoint, direction) plane =
    Map.adjust ((rootPoint, stepsFromRootPoint, direction) :) destination plane

plotPoints :: Plane -> [PointQuadriple] -> Plane
plotPoints plane (x : xs) = plotPoints (putPointOnPlane x plane) xs
plotPoints plane [] = plane

-- each step (that /=0) allocates (4 * stepFromRoot) points
fillLocations :: Plane -> [PointQuadriple] -> StepsFromRootPoint -> Plane
fillLocations plane pointQuadplesList steps
    | steps == 5 = plane
    | True = fillLocations (plotPoints plane pointQuadplesList) adjacentPointQuadriples (steps + 1) where
        adjacentPointQuadriples = concat $ map makeAdjacentPointQuadriples pointQuadplesList

makeAdjacentPointQuadriples :: PointQuadriple -> [PointQuadriple]
makeAdjacentPointQuadriples ([x,y], rootPoint, steps, direction)
    | steps == 0 = 
        [
            ([x, y + 1], rootPoint, steps + 1, "N"),
            ([x + 1, y], rootPoint, steps + 1, "E"),
            ([x, y - 1], rootPoint, steps + 1, "S"),
            ([x - 1, y], rootPoint, steps + 1, "W")
        ]

    -- clockwork turns; stepsFromRoot will be = 2
    | direction == "N" = 
        [
            ([x, y + 1], rootPoint, steps + 1, "N"),
            ([x + 1, y], rootPoint, steps + 1, "NE")
        ]

    | direction == "E" = 
        [
            ([x + 1, y], rootPoint, steps + 1, "E"),
            ([x, y - 1], rootPoint, steps + 1, "SE")
        ]

    | direction == "S" =
        [
            ([x, y - 1], rootPoint, steps + 1, "S"),
            ([x - 1, y], rootPoint, steps + 1, "SW")
        ]

    | direction == "W" =
        [
            ([x - 1, y], rootPoint, steps + 1, "W"),
            ([x, y + 1], rootPoint, steps + 1, "NW")
        ]

    --  stepsFromRoot will be > 2
    | direction == "NE" =
        [
            ([x + 1, y], rootPoint, steps + 1, "NE")
        ]

    | direction == "SE" =
        [
            ([x, y - 1], rootPoint, steps + 1, "SE")
        ]

    | direction == "SW" =
        [
            ([x - 1, y], rootPoint, steps + 1, "SW")
        ]

    | direction == "NW" =
        [
            ([x, y + 1], rootPoint, steps + 1, "NW")
        ]

pointToTuple :: Point -> ((Int, Int), Int)
pointToTuple [x, y] = ((x, y), 0)

makePointsAreaCounter :: [Point] -> Map.Map (Int, Int) Int
makePointsAreaCounter = Map.fromList . map pointToTuple

main = mainWith myF
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          --solveFirstPuzzlePart = show . findBoundingPoints . map strToPoint . lines
          --solveFirstPuzzlePart = show . boundingPointsExtremeCoords . findBoundingPoints . map strToPoint . lines

          --solveFirstPuzzlePart input = show $ plotPoints plane pointQuadplesList where
          solveFirstPuzzlePart input = show $ filterByRootPoint $ fillLocations plane pointQuadplesList 0 where

            filterByRootPoint :: Plane -> Plane
            filterByRootPoint = Map.filter (not . null . (filter filterByPointInfo)) 

            filterByPointInfo :: (RootPoint, StepsFromRootPoint, Direction) -> Bool
            filterByPointInfo (rootPoint, stepsFromRoot, _) = rootPoint == [54,185] && stepsFromRoot == 4

            pointsList = map strToPoint $ lines input
            plane = makePlane $ boundingPointsExtremeCoords $ findBoundingPoints pointsList
            pointQuadplesList = map prepareRootPointToPutOnPlane pointsList

          --solveFirstPuzzlePart = show . makePointsAreaCounter . map strToPoint . lines
          --solveFirstPuzzlePart = show . foldl makePointsMap Map.empty . map strToPoint . lines

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
