import itertools
import cython



@cython.boundscheck(False)
@cython.wraparound(False)
cdef get_neighbors((int, int) cell):

    cdef (int, int) cells[8]
    cells[0] = (cell[0]-1, cell[1])
    cells[1] = (cell[0]+1, cell[1])
    cells[2] = (cell[0], cell[1]-1)
    cells[3] = (cell[0], cell[1]+1)
    cells[4] = (cell[0]-1, cell[1]-1)
    cells[5] = (cell[0]+1, cell[1]-1)
    cells[6] = (cell[0]-1, cell[1]+1)
    cells[7] = (cell[0]+1, cell[1]+1)
    return cells


@cython.boundscheck(False)
@cython.wraparound(False)
cdef check_alive((int, int) cell, living_cells):
    """For live cells"""
    # cdef (int, int) neighbors[8]
    neighbors = set(get_neighbors(cell))
    live_neighbors = len(neighbors.intersection(living_cells))
    if live_neighbors < 2 or live_neighbors == 4:
        return False
    else:
        return True


@cython.boundscheck(False)
@cython.wraparound(False)
cdef check_becomes_alive((int, int) cell, living_cells):
    neighbors = set(get_neighbors(cell))
    live_neighbors = len(neighbors.intersection(living_cells))
    if live_neighbors == 3:
        return True
    else:
        return False


@cython.boundscheck(False)
@cython.wraparound(False)
def print_board(living_cells):
    b0_min = min(living_cells, key=lambda x: x[0])[0]
    b1_min = min(living_cells, key=lambda x: x[1])[1]
    b0_max = max(living_cells, key=lambda x: x[0])[0]
    b1_max = max(living_cells, key=lambda x: x[1])[1]
    print(living_cells)

    out = '\n'.join(
        [' '.join(
            ['+' if (i, j) in living_cells else '.'
             for j in range(b1_min, b1_max+1)])
            for i in range(b0_min, b0_max+1)]
        )
    print(out)


def play(steps):
    living_cells = set([(0, 0),
                        (0, 1),
                        (0, 5),
                        (1, 0),
                        (1, 6),
                        (2, 1),
                        (2, 4),
                        (2, 5),
                        (2, 6)])

    cdef int i
    for i in range(steps):
        new_living_cells = filter(lambda x: check_alive(x, living_cells), living_cells)
        all_neighbors = map(get_neighbors, living_cells)
        all_neighbors = set(itertools.chain.from_iterable(all_neighbors))
        newly_living_cells = filter(lambda x: check_becomes_alive(x, living_cells), all_neighbors)
        living_cells = set(new_living_cells).union(newly_living_cells)
        # print_board(living_cells)
