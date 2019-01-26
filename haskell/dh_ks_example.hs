import           Data.Array as Array
import           Data.Ix    as Ix
import qualified Data.Set   as Set

type Coords = (Int, Int)

type Grid = Array Coords Int

type Component = Set.Set Coords

example :: Grid
example = Array.listArray ((0, 0), (2, 3)) [1, 1, 2, 3, 1, 2, 3, 2, 3, 2, 2, 2]

neighbors :: Grid -> Coords -> [Coords]
neighbors grid (r, c) =
  filter
    (Ix.inRange (Array.bounds grid))
    [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

sameTypeNeighbors :: Grid -> Coords -> [Coords]
sameTypeNeighbors grid coords =
  filter
    (\neighborIx -> (grid ! neighborIx) == (grid ! coords))
    (neighbors grid coords)

connectedComponent :: Grid -> Set.Set Coords -> Coords -> Component
connectedComponent grid visited ix =
  let unvisitedNeighbors =
        filter (`Set.notMember` visited) (sameTypeNeighbors grid ix)
      updatedVisited = Set.insert ix visited
      neighboringComponents =
        map (connectedComponent grid updatedVisited) unvisitedNeighbors
   in Set.insert ix (Set.unions neighboringComponents)

maxComponent :: Grid -> [Component] -> Int
maxComponent grid components =
  let visited = Set.unions components
      unvisited = filter (`Set.notMember` visited) (Array.indices grid)
   in case unvisited of
        [] -> maximum (map Set.size components)
        x:_ ->
          maxComponent grid (connectedComponent grid visited x : components)

main :: IO ()
main = print (maxComponent example [])
