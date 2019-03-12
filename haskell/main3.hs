-- New try: With lists and maps
import qualified Data.Map.Strict as Map


type Cell = (Int, Int)
type CellSet = [Cell]


square :: CellSet
square = [(0, 0), (0, 1), (1, 0), (1, 1)]


population :: CellSet
population = [(0, 0), (0, 1), (0, 5), (1, 0), (1, 6), (2, 1), (2, 4), (2, 5), (2, 6)]


getNeighbors :: Cell -> [Cell]
getNeighbors (ix, iy) =
    [(ix - 1, iy - 1),
     (ix - 1, iy),
     (ix - 1, iy + 1),
     (ix, iy - 1),
     (ix, iy + 1),
     (ix + 1, iy - 1),
     (ix + 1, iy),
     (ix + 1, iy + 1)]


getNeighborCounts :: CellSet -> Map.Map Cell Int
getNeighborCounts livingCells =
  Map.fromListWith (+)
  (map (\a -> (a, 1)) (
      concat (map getNeighbors livingCells)))


-- newborns :: CellSet -> CellSet
-- newborns livingCells =
--   Map.keys (Map.filter (== 3) (getNeighborCounts livingCells))


-- survivors :: CellSet -> CellSet
-- survivors livingCells =
--   filter
--   (\cell -> Map.findWithDefault 0 cell (getNeighborCounts livingCells) == 2)
--   livingCells


nextPopulation :: CellSet -> CellSet
nextPopulation livingCells =
  let
    neighborCounts = getNeighborCounts livingCells
    newborns = Map.keys (Map.filter (== 3) neighborCounts)
    survivors = filter
      (\cell -> Map.findWithDefault 0 cell neighborCounts == 2)
      livingCells
  in
    survivors ++ newborns


nStepSimulation :: Int -> CellSet -> CellSet
nStepSimulation steps livingCells =
  case steps of
    0 -> livingCells
    _ -> nStepSimulation (steps-1) (nextPopulation livingCells)


main :: IO ()
-- main = print (population)
-- main = print (getNeighbors (0, 0))
-- main = print (becomesAlive population (-1, -1))
-- main = print (getAllNeighbors square)
-- main = print (nextPopulation square)
main = print (nStepSimulation 100000 population)
