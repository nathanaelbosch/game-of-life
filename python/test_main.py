import pytest

from main import *


square = set([(0, 0), (0, 1), (1, 0), (1, 1)])


def test_get_neighbors():
    cell = (0, 0)
    neighbors = set([
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ])
    assert neighbors == get_neighbors(cell)


def test_check_alive():
    for cell in square:
        assert check_alive(cell, square)


def test_check_becomes_alive():
    living_cells = square.copy()
    living_cells.remove((0, 0))
    assert check_becomes_alive((0, 0), living_cells)


def test_compute_next_generation():
    assert square == compute_next_generation(square)
