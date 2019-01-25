import itertools


living_cells = set([(0, 0),
                    (0, 1),
                    (0, 5),
                    (1, 0),
                    (1, 6),
                    (2, 1),
                    (2, 4),
                    (2, 5),
                    (2, 6)])

# living_cells = set([(0, 0),
#                     (0, 1),
#                     (0, -1)])


def get_neighbors(cell):
    return set([(cell[0]-1, cell[1]),
                (cell[0]+1, cell[1]),
                (cell[0], cell[1]-1),
                (cell[0], cell[1]+1),
                (cell[0]-1, cell[1]-1),
                (cell[0]+1, cell[1]-1),
                (cell[0]-1, cell[1]+1),
                (cell[0]+1, cell[1]+1),
    ])


def check_alive(cell, living_cells):
    """For live cells"""
    neighbors = get_neighbors(cell)
    live_neighbors = len(neighbors.intersection(living_cells))
    if live_neighbors < 2 or live_neighbors == 4:
        return False
    else:
        return True


def check_becomes_alive(cell, living_cells):
    neighbors = get_neighbors(cell)
    live_neighbors = len(neighbors.intersection(living_cells))
    if live_neighbors == 3:
        return True
    else:
        return False


def print_board(living_cells):
    b0_min = min(living_cells, key=lambda x: x[0])[0]
    b1_min = min(living_cells, key=lambda x: x[1])[1]
    b0_max = max(living_cells, key=lambda x: x[0])[0]
    b1_max = max(living_cells, key=lambda x: x[1])[1]
    print(living_cells)

    for i in range(b0_min, b0_max+1):
        for j in range(b1_min, b1_max+1):
            if (i, j) in living_cells:
                print('+', end=" ")
            else:
                print('.', end=" ")
        print()


for _ in range(1000):
    new_living_cells = filter(lambda x: check_alive(x, living_cells), living_cells)
    all_neighbors = map(get_neighbors, living_cells)
    all_neighbors = set(itertools.chain.from_iterable(all_neighbors))
    newly_living_cells = filter(lambda x: check_becomes_alive(x, living_cells), all_neighbors)
    living_cells = set(new_living_cells).union(newly_living_cells)
    print_board(living_cells)
