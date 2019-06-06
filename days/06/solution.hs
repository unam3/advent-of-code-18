import System.Environment (getArgs)
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input


type Point = (Int, Int)

strToPoint :: String -> Point
strToPoint = strToInts . words where
    strToInts [x, y] = (read $ init x, read y)
    strToInts _ = error "strToPoint: takes only string with one space between numbers"

makePlanePointsList :: (Int, Int, Int, Int) -> [Point]
makePlanePointsList (minX, maxX, minY, maxY) = makePlaneList [] minX minY where
    makePlaneList acc x y
        | x < maxX = makePlaneList ((x, y) : acc) (x + 1) y
        | y < maxY = makePlaneList ((x, y) : acc) minX (y + 1)
        | y == maxY && x == maxX = ((x, y) : acc)
        | otherwise = acc

type RootPoint = (Int, Int)

type StepsFromRootPoint = Int

type Direction = String

type PointQuadriple = (Point, RootPoint, StepsFromRootPoint, Direction)

prepareRootPointToPutOnPlane :: Point -> PointQuadriple
prepareRootPointToPutOnPlane point = (point, point, 0, "")


findBoundingPoints :: [Point] -> (Point, Point, Point, Point)
findBoundingPoints pointsList = (minXPoint, maxXPoint, minYPoint, maxYPoint) where
    minXPoint = List.minimumBy (Ord.comparing fst) pointsList
    maxXPoint = List.maximumBy (Ord.comparing fst) pointsList
    minYPoint = List.minimumBy (Ord.comparing snd) pointsList
    maxYPoint = List.maximumBy (Ord.comparing snd) pointsList

getBoundingPointsExtremeCoords :: (Point, Point, Point, Point) -> (Int, Int, Int, Int)
getBoundingPointsExtremeCoords (minXPoint, maxXPoint, minYPoint, maxYPoint) = (minX, maxX, minY, maxY) where
    minX = fst minXPoint
    maxX = fst maxXPoint
    minY = snd minYPoint
    maxY = snd maxYPoint

type PointInfo = (RootPoint, StepsFromRootPoint, Direction)

type Plane = Map.Map Point [PointInfo]


makePlane :: [Point] -> Plane
makePlane = Map.fromList . map (\point -> (point, []))

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


-- coordinate — any root point
-- location — any point
countDistanceToCoordinate :: Point -> Point -> Int
countDistanceToCoordinate (lx, ly) (cx, cy) = abs (lx - cx) + abs (ly - cy)

countTotalDistance :: Point -> [Point] -> Int
countTotalDistance location = foldl f 0 where
    f totalDistanceAcc coordinate = totalDistanceAcc + (countDistanceToCoordinate location coordinate)


type MaxTotalDistance = Int

type PointsList = [Point]

isLocationWithinDesiredRegion :: MaxTotalDistance -> PointsList -> Point -> Bool
isLocationWithinDesiredRegion maxTotalDistance coordinates location = totalDistance < maxTotalDistance where
    totalDistance = countTotalDistance location coordinates


type PlanePointsList = [Point]

type Region = [Point]

findParticulartRegionSize :: MaxTotalDistance -> PointsList -> PlanePointsList -> Int
findParticulartRegionSize maxTotalDistance pointsList =
    length . filter (isLocationWithinDesiredRegion maxTotalDistance pointsList)


main :: IO ()
main = mainWith myF
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          --solveFirstPuzzlePart input = show . getBoundingPointsExtremeCoords . findBoundingPoints . map strToPoint $ lines input where
          solveFirstPuzzlePart input = show $ getLargestAreaSize $ filterInfiniteAreasPoints filledPlane  where
          --solveFirstPuzzlePart input = show $ possibleBorderPoints boundingPointsExtremeCoords where
            filledPlane = fillLocations plane pointQuadplesList 0
            pointsList = map strToPoint $ lines input
            plane = makePlane $ makePlanePointsList boundingPointsExtremeCoords
            boundingPointsExtremeCoords = getBoundingPointsExtremeCoords boundingPoints
            boundingPoints = findBoundingPoints pointsList
            pointQuadplesList = map prepareRootPointToPutOnPlane pointsList
            
            filterInfiniteAreasPoints :: Plane -> Plane
            filterInfiniteAreasPoints = Map.filter (all (not . ((`elem` infiniteAreasRootPoints) . fst')))

            infiniteAreasRootPoints :: [RootPoint]
            infiniteAreasRootPoints =
                foldl infiniteAreasPointsFoldl [] (possibleBorderPoints boundingPointsExtremeCoords)

            infiniteAreasPointsFoldl :: [Point] -> Point -> [RootPoint]
            infiniteAreasPointsFoldl borderPoints point
                | maybeRootPoint == Nothing = borderPoints
                | otherwise = ((fst' $ head $ Maybe.fromJust maybeRootPoint) : borderPoints) where
                    maybeRootPoint = Map.lookup point filledPlane

            possibleBorderPoints :: (Int, Int, Int, Int) -> [Point]
            possibleBorderPoints (minX, maxX, minY, maxY) = 
                zip [minX..maxX] (repeat minY) ++
                zip [minX..maxX] (repeat maxY) ++
                zip (repeat minX) [minY..maxY] ++
                zip (repeat maxX) [minY..maxY]


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

          solveSecondPuzzlePart input = show $ findParticulartRegionSize maxTotalDistance pointsList planePointsList where
            maxTotalDistance :: Int
            --maxTotalDistance = 32
            maxTotalDistance = 10000

            planePointsList = makePlanePointsList boundingPointsExtremeCoords
            boundingPointsExtremeCoords = getBoundingPointsExtremeCoords boundingPoints
            boundingPoints = findBoundingPoints pointsList
            pointsList = map strToPoint $ lines input


          solvePuzzle input = "First part solution is: " ++ solveFirstPuzzlePart input ++ "\n" ++
            "Second part solution is: " ++ solveSecondPuzzlePart input
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

--- Part Two ---

-- On the other hand, if the coordinates are safe, maybe the best you can do is try to find a region near as many coordinates as possible.
-- 
-- For example, suppose you want the sum of the Manhattan distance to all of the coordinates to be less than 32. For each location, add up the distances to all of the given coordinates; if the total of those distances is less than 32, that location is within the desired region. Using the same coordinates as above, the resulting region looks like this:
-- 
-- ..........
-- .A........
-- ..........
-- ...###..C.
-- ..#D###...
-- ..###E#...
-- .B.###....
-- ..........
-- ..........
-- ........F.
-- In particular, consider the highlighted location 4,3 located at the top middle of the region. Its calculation is as follows, where abs() is the absolute value function:
-- 
-- Distance to coordinate A: abs(4-1) + abs(3-1) =  5
-- Distance to coordinate B: abs(4-1) + abs(3-6) =  6
-- Distance to coordinate C: abs(4-8) + abs(3-3) =  4
-- Distance to coordinate D: abs(4-3) + abs(3-4) =  2
-- Distance to coordinate E: abs(4-5) + abs(3-5) =  3
-- Distance to coordinate F: abs(4-8) + abs(3-9) = 10
-- Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
-- Because the total distance to all coordinates (30) is less than 32, the location is within the region.
-- 
-- This region, which also includes coordinates D and E, has a total size of 16.
-- 
-- Your actual region will need to be much larger than this example, though, instead including all locations with a total distance of less than 10000.
-- 
-- What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
