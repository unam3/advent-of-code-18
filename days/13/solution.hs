{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

type TrackElementPosition = (Int, Int)

data TrackElement = V  |
                    H  |
                    S  |
                    BS |
                    I deriving Show

type Track = Map.Map TrackElementPosition TrackElement


--type CartPosition = TrackElementPosition

--data IntersectionTurn = Left | Straight | Right

--type Carts = [(CartPosition, IntersectionTurn)]

--data CartsOnTracks = CartsOnTracks Track Carts


charToTrackElement :: Char -> TrackElement
charToTrackElement '|'  = V
charToTrackElement '-'  = H
charToTrackElement '/'  = S
charToTrackElement '\\' = BS
charToTrackElement '+'  = I
charToTrackElement _  = undefined

insertIntoTrack :: Track -> (TrackElementPosition, Char) -> Track
insertIntoTrack track (_, ' ') = track
-- related to cart direction
insertIntoTrack track (k, '^') = insertIntoTrack track (k, '|')
insertIntoTrack track (k, 'v') = insertIntoTrack track (k, '|')
insertIntoTrack track (k, '<') = insertIntoTrack track (k, '-')
insertIntoTrack track (k, '>') = insertIntoTrack track (k, '-')
--
insertIntoTrack track (k, v) =
    let {
        mappedV = charToTrackElement v;
    } in Map.insert k mappedV track


processString :: (Int, [(Int, Char)]) -> Track
processString (y, xAndCharPairsList) = 
    let {
        initialTrack = Map.empty :: Track;
    } in List.foldl' insertIntoTrack initialTrack $
        fmap (\(x, v) -> ((x, y), v)) xAndCharPairsList

createTrack :: [String] -> Track
createTrack strings =
    let {
        numbersList = [0..] :: [Int];
    } in List.foldl1 Map.union $ fmap processString (zip numbersList $ fmap (zip numbersList) strings)

parseInput :: String -> Track
parseInput = createTrack . lines


getFirstCrashLocation :: String -> Track
getFirstCrashLocation = parseInput


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
              "First part solution is: " ++ firstPuzzlePart where
              -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart where
              --"Second part solution is: " List.++ secondPuzzlePart where
                firstPuzzlePart = show $ getFirstCrashLocation input
                --secondPuzzlePart =

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

/---\        
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
|   |  /---<\
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
