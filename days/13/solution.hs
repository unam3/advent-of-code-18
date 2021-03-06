import Prelude hiding (Left, Right)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

type Position = (Int, Int)

data TrackElement = V  |
                    H  |
                    S  |
                    BS |
                    I deriving (Show, Eq)

type Track = Map.Map Position TrackElement


data IntersectionTurn = TurnLeft |
                        GoStraight |
                        TurnRight deriving Show

data HeadingDirection = Up |
                        Down |
                        Left |
                        Right deriving (Show, Eq)

type Cart = (Position, IntersectionTurn, HeadingDirection)
type Carts = [Cart]

data CartsOnTracks = CartsOnTracks Track Carts deriving Show


charToTrackElement :: Char -> TrackElement
charToTrackElement '|'  = V
charToTrackElement '-'  = H
charToTrackElement '/'  = S
charToTrackElement '\\' = BS
charToTrackElement '+'  = I
charToTrackElement _  = undefined

insertIntoTrack :: Track -> (Position, Char) -> Track
insertIntoTrack track (_, ' ') = track
insertIntoTrack track (k, '^') = insertIntoTrack track (k, '|')
insertIntoTrack track (k, 'v') = insertIntoTrack track (k, '|')
insertIntoTrack track (k, '<') = insertIntoTrack track (k, '-')
insertIntoTrack track (k, '>') = insertIntoTrack track (k, '-')
insertIntoTrack track (k, v) =
    let {
        mappedV = charToTrackElement v;
    } in Map.insert k mappedV track

processCartInfo :: Carts -> (Position, Char) -> Carts
processCartInfo carts (k, '^') = (k, TurnLeft, Up):carts
processCartInfo carts (k, 'v') = (k, TurnLeft, Down):carts
processCartInfo carts (k, '>') = (k, TurnLeft, Right):carts
processCartInfo carts (k, '<') = (k, TurnLeft, Left):carts
processCartInfo carts _ = carts

processString' :: CartsOnTracks -> (Position, Char) -> CartsOnTracks
processString' (CartsOnTracks track carts) (k, v) =
    let {
        newTrack = insertIntoTrack track (k, v);
        newCarts = processCartInfo carts (k, v);
    } in CartsOnTracks newTrack newCarts

processString :: (Int, [(Int, Char)]) -> CartsOnTracks
processString (y, xAndCharPairsList) = 
    let {
        initialCartsOnTracks = CartsOnTracks (Map.empty :: Track) ([] :: Carts);
    } in List.foldl' processString' initialCartsOnTracks $
        fmap (\(x, v) -> ((x, y), v)) xAndCharPairsList

mergeState :: CartsOnTracks -> CartsOnTracks -> CartsOnTracks
mergeState (CartsOnTracks track carts) (CartsOnTracks track' carts') =
    let {
        newTrack = Map.union track track';
        newCarts =
            carts List.++ carts';
    } in CartsOnTracks newTrack newCarts

createTrack :: [String] -> CartsOnTracks
createTrack strings =
    let {
        numbersList = [0..] :: [Int];
    } in List.foldl1 mergeState $ fmap processString (zip numbersList $ fmap (zip numbersList) strings)


sortCarts :: Carts -> Carts
sortCarts =
    List.sortBy (\((x, _), _, _) ((x1, _), _, _) -> compare x x1) .
    List.sortBy (\((_, y), _, _) ((_, y1), _, _) -> compare y y1)


getNextPosition :: HeadingDirection -> Position -> Position
getNextPosition headingDirection (x, y) =
    case headingDirection of
         Right -> (x + 1, y)
         Down  -> (x, y + 1)
         Left  -> (x - 1, y)
         _     -> (x, y - 1)

getNextIntersectionTurn :: IntersectionTurn -> IntersectionTurn
getNextIntersectionTurn TurnLeft = GoStraight
getNextIntersectionTurn GoStraight = TurnRight
getNextIntersectionTurn _ = TurnLeft

getNextHeadingDirection :: TrackElement -> IntersectionTurn -> HeadingDirection -> HeadingDirection
getNextHeadingDirection nextTrackElement intersectionTurn headingDirection = 
    case nextTrackElement of
        S -> case headingDirection of
            Down  -> Left
            Up    -> Right
            Right -> Up
            _     -> Down
        BS -> case headingDirection of
            Down  -> Right
            Up    -> Left
            Right -> Down
            _     -> Up
        I -> case headingDirection of
                  Up    -> case intersectionTurn of
                                TurnLeft   -> Left
                                GoStraight -> Up
                                TurnRight  -> Right
                  Down  -> case intersectionTurn of
                                TurnLeft   -> Right
                                GoStraight -> Down
                                TurnRight  -> Left
                  Right -> case intersectionTurn of
                                TurnLeft   -> Up
                                GoStraight -> Right
                                TurnRight  -> Down
                  _     -> case intersectionTurn of
                                TurnLeft   -> Down
                                GoStraight -> Left
                                TurnRight  -> Up
        _ -> headingDirection

moveCart :: Cart -> Track -> Cart
moveCart (cartPosition, intersectionTurn, headingDirection) track =
    let {
        nextPosition = getNextPosition headingDirection cartPosition;
        nextTrackElement = track Map.! nextPosition;
        nextIntersectionTurn =
            if nextTrackElement == I
            then getNextIntersectionTurn intersectionTurn
            else intersectionTurn;
        nextHeadingDirection = getNextHeadingDirection nextTrackElement intersectionTurn headingDirection;
    } in (nextPosition, nextIntersectionTurn, nextHeadingDirection) 

moveCarts :: CartsOnTracks -> CartsOnTracks
moveCarts (CartsOnTracks track carts) =
    let {
        flippedMoveCart = flip moveCart;
        newCarts = fmap (flippedMoveCart track) $ sortCarts carts;
    } in CartsOnTracks track newCarts

parseInput :: String -> CartsOnTracks
parseInput = createTrack . lines


--cartsHasCrash :: Carts -> Either Carts Position
collectCrashPositions :: Carts -> [Position]
collectCrashPositions = Map.keys . Map.filter (> (1 :: Int)) .
    List.foldl' (\acc (position, _, _) -> Map.insertWith (+) position (1 :: Int) acc) Map.empty


moveCartsUntilCrash :: CartsOnTracks -> [Position]
moveCartsUntilCrash cartsOnTracks@(CartsOnTracks track _) =
    let {
        (CartsOnTracks _ movedCarts) = moveCarts cartsOnTracks;
        crashPositions = collectCrashPositions movedCarts;
        hasCartsCrash = not $ null crashPositions;
    } in if hasCartsCrash
         then crashPositions
         else moveCartsUntilCrash (CartsOnTracks track movedCarts)


--getFirstCrashLocation :: String -> Either CartsOnTracks Position
--getFirstCrashLocation :: String -> CartsOnTracks
--getFirstCrashLocation = moveCarts . parseInput
getFirstCrashLocation :: String -> [Position]
getFirstCrashLocation = moveCartsUntilCrash . parseInput
--getFirstCrashLocation :: String -> CartsOnTracks
--getFirstCrashLocation = moveCarts . moveCarts . moveCarts . moveCarts . parseInput


filterCrashedCarts :: Carts -> [Position] -> Carts
filterCrashedCarts carts crashPositions = List.filter (\(position, _, _) -> List.all (/= position) crashPositions) carts

getOneCartPosition :: CartsOnTracks -> Position
getOneCartPosition (CartsOnTracks _ carts) = (\(position, _, _) -> position) $ head carts

moveCartsUntilCrashButLastCart :: CartsOnTracks -> Position
moveCartsUntilCrashButLastCart cartsOnTracks@(CartsOnTracks track _) =
    let {
        (CartsOnTracks _ movedCarts) = moveCarts cartsOnTracks;
        crashPositions = collectCrashPositions movedCarts;
        withoutCrashedCarts = filterCrashedCarts movedCarts crashPositions;
        hasOnlyOneCart = 1 == (length withoutCrashedCarts);
    } in if hasOnlyOneCart
         then getOneCartPosition $ moveCarts (CartsOnTracks track withoutCrashedCarts)
         else moveCartsUntilCrashButLastCart (CartsOnTracks track withoutCrashedCarts)

getLastCartLocation :: String -> Position
getLastCartLocation = moveCartsUntilCrashButLastCart . parseInput

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

          solvePuzzle input = 
              --"First part solution is: " ++ firstPuzzlePart where
              -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
              "Second part solution is: " List.++ secondPuzzlePart where
                --firstPuzzlePart = show $ getFirstCrashLocation input
                secondPuzzlePart = show $ getLastCartLocation input

{-
--- Day 13: Mine Cart Madness ---

A crop of this size requires significant logistics to transport produce, soil, fertilizer, and so on. The Elves are very busy pushing things around in carts on some kind of rudimentary system of tracks they've come up with.

Seeing as how cart-and-track systems don't appear in recorded history for another 1000 years, the Elves seem to be making this up as they go along. They haven't even figured out how to avoid collisions yet.

You map out the tracks (your puzzle input) and see where you can help.

Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect exactly two perpendicular pieces of track; for example, this is a closed loop:

/----\
|    |
|    |
\----/
Intersections occur when two perpendicular paths cross. At an intersection, a cart is capable of turning left, turning right, or continuing straight. Here are two loops connected by two intersections:

/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/
Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right (>). (On your initial map, the track under each cart is a straight path matching the direction the cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection), it turns left the first time, goes straight the second time, turns right the third time, and then repeats those directions starting again with left the fourth time, straight the fifth time, and so on. This process is independent of the particular intersection at which the cart has arrived - that is, the cart has no per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a time. They do this based on their current location: carts on the top row move first (acting from left to right), then carts on the second row move (again from left to right), then carts on the third row, and so on. Once each cart has moved one step, the process repeats; each of these loops is called a tick.

For example, suppose there are two carts on a straight track:

|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |
First, the top cart moves. It is facing down (v), so it moves down one square. Second, the bottom cart moves. It is facing up (^), so it moves up one square. Because all carts have moved, the first tick ends. Then, the process repeats, starting with the first cart. The first cart moves down, then the second cart moves up - right into the first cart, colliding with it! (The location of the crash is marked with an X.) This ends the second and last tick.

Here is a longer example:

/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   

/-->\        
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \->--/
  \------/   

/---v        
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+>-/
  \------/   

/---\        
|   v  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+->/
  \------/   

/---\        
|   |  /----\
| /->--+-\  |
| | |  | |  |
\-+-/  \-+--^
  \------/   

/---\         5
|   |  /----\
| /-+>-+-\  |
| | |  | |  ^
\-+-/  \-+--/
  \------/   

/---\        
|   |  /----\
| /-+->+-\  ^
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /---<\ 8
| /-+--+>\  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /--<-\
| /-+--+-v  |
| | |  | |  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /-<--\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   

/---\        
|   |  /<---\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-<--/
  \------/   

/---\        
|   |  v----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \<+--/
  \------/   

/---\        
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/   

/---\        
|   |  /----\
| /-+--+-\  |
| | |  X |  |
\-+-/  \-+--/
  \------/   
After following their respective paths for a while, the carts eventually crash. To help prevent crashes, you'd like to know the location of the first crash. Locations are given in X,Y coordinates, where the furthest left column is X=0 and the furthest top row is Y=0:

           111
 0123456789012
0/---\        
1|   |  /----\
2| /-+--+-\  |
3| | |  X |  |
4\-+-/  \-+--/
5  \------/   
In this example, the location of the first crash is 7,3.
-}
