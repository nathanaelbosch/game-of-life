import qualified Data.Set   as Set


type Cell = (Int, Int)
type CellSet = Set.Set Cell


square :: CellSet
square = Set.fromList [(0, 0), (0, 1), (1, 0), (1, 1)]


population :: CellSet
population = Set.fromList [(0, 0),
                           (0, 1),
                           (0, 5),
                           (1, 0),
                           (1, 6),
                           (2, 1),
                           (2, 4),
                           (2, 5),
                           (2, 6)]


getNeighbors :: Cell -> CellSet
getNeighbors (ix, iy) =
  Set.fromList
    [(ix - 1, iy - 1),
     (ix - 1, iy),
     (ix - 1, iy + 1),
     (ix, iy - 1),
     (ix, iy + 1),
     (ix + 1, iy - 1),
     (ix + 1, iy),
     (ix + 1, iy + 1)]


countLivingNeighbors :: CellSet -> Cell -> Int
countLivingNeighbors livingCells cell =
  Set.size (Set.intersection (getNeighbors cell) livingCells)


becomesAlive :: CellSet -> Cell -> Bool
becomesAlive livingCells cell =
  case countLivingNeighbors livingCells cell of
    3 -> True
    _ -> False


staysAlive :: CellSet -> Cell -> Bool
staysAlive livingCells cell =
  case countLivingNeighbors livingCells cell of
    2 -> True
    3 -> True
    _ -> False


nextPopulation :: CellSet -> CellSet
nextPopulation livingCells =
  let
    stillLivingCells = Set.filter (staysAlive livingCells) livingCells

    deadNeighbors = Set.difference (Set.unions(Set.map getNeighbors livingCells)) livingCells
    newlyLivingCells = Set.filter (becomesAlive livingCells) deadNeighbors
  in Set.union newlyLivingCells stillLivingCells


nStepSimulation :: Int -> CellSet -> CellSet
nStepSimulation steps livingCells =
  case steps of
    0 -> livingCells
    _ -> nStepSimulation (steps-1) (nextPopulation livingCells)


-- visualize :: CellSet -> IO ()
-- visualize CellSet =


main :: IO ()
-- main = print (population)
-- main = print (getNeighbors (0, 0))
-- main = print (becomesAlive population (-1, -1))
-- main = print (nextPopulation population)
main = print (nStepSimulation 100000 population)
