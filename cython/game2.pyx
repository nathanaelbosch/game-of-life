# distutils: language=c++
import cython
from collections import defaultdict

from libcpp.vector cimport vector


@cython.boundscheck(False)
@cython.wraparound(False)
cdef vector[vector[int]] get_neighbors(vector[int] cell):
    cdef vector[vector[int]] cells
    cells.reserve(8)
    cells[0] = [cell[0]-1, cell[1]]
    cells[1] = [cell[0]+1, cell[1]]
    cells[2] = [cell[0], cell[1]-1]
    cells[3] = [cell[0], cell[1]+1]
    cells[4] = [cell[0]-1, cell[1]-1]
    cells[5] = [cell[0]+1, cell[1]-1]
    cells[6] = [cell[0]-1, cell[1]+1]
    cells[7] = [cell[0]+1, cell[1]+1]
    return cells


@cython.boundscheck(False)
@cython.wraparound(False)
cdef count_alive_neighbors(vector[vector[int]] living_cells):
    """Return a map which contains the counts of alive neighbors"""
    cdef vector[int] _c
    neighbors = [get_neighbors(_c) for _c in living_cells]

    cdef vector[vector[int]] _nbs
    count_map = defaultdict(int)
    for _nbs in neighbors:
        for _c in _nbs:
            count_map[tuple(_c)] += 1
    return count_map


@cython.boundscheck(False)
@cython.wraparound(False)
cdef vector[vector[int]] next_gen_cells(vector[vector[int]] living_cells):
    count_map = count_alive_neighbors(living_cells)
    cdef vector[int] c
    cdef vector[vector[int]] out

    for c in living_cells:
        if count_map[tuple(c)] == 2:
            out.push_back(c)
    for c in count_map.keys():
        if count_map[tuple(c)] == 3:
            out.push_back(c)
    return out


@cython.boundscheck(False)
@cython.wraparound(False)
cpdef main(int steps):
    cdef vector[vector[int]] living_cells = [
        [0, 0],
        [0, 1],
        [0, 5],
        [1, 0],
        [1, 6],
        [2, 1],
        [2, 4],
        [2, 5],
        [2, 6]]
    print(living_cells)
    print(get_neighbors(living_cells[0]))
    cdef int s = 0
    for s in range(steps):
        print(living_cells)
        living_cells = next_gen_cells(living_cells)

    print(living_cells)
