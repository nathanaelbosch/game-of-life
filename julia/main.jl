import StatsBase: countmap


function get_neighbors(cell)
    return [(cell[1]-1, cell[2]),
            (cell[1]+1, cell[2]),
            (cell[1], cell[2]-1),
            # (cell[1], cell[2]),
            (cell[1], cell[2]+1),
            (cell[1]-1, cell[2]-1),
            (cell[1]+1, cell[2]-1),
            (cell[1]-1, cell[2]+1),
            (cell[1]+1, cell[2]+1),
            ]
end


"""Count the occurrence of each neighbor

This is equivalent to counting the next living cells for each neighbor!
`StatsBase.countmap` does all the magic and counts occurrences of items in a collection.
"""
get_neighbor_counts(living_cells) =
    countmap(vcat(get_neighbors.(living_cells)...))


"""Compute the next population

The neighbor_counts include all potential living cells.
1. If a cell is not in there, it will not live!
2. If a cell has 3 neighbors, it lives!
3. If a cell has 2 neighbors and is alive, it survives!
"""
function next_population(current_population)
    neighbor_counts = get_neighbor_counts(current_population)

    return filter(neighbor_counts) do (k, v)
        v == 3 || (k in current_population && v == 2)
    end |> keys
end


function print_board(living_cells)
    b0_min = minimum(living_cells) do cell
        cell[1]
    end
    b1_min = minimum(living_cells) do cell
        cell[2]
    end
    b0_max = maximum(living_cells) do cell
        cell[1]
    end
    b1_max = maximum(living_cells) do cell
        cell[2]
    end

    println(living_cells)

    board_str = join([join([(i, j) in living_cells ? '+' : '.' for j in b1_min:b1_max],
          "")
          for i in b0_min:b0_max],
         "\n")
    println(board_str)
end



let living_cells=[(0, 0),
                      (0, 1),
                      (0, 5),
                      (1, 0),
                      (1, 6),
                      (2, 1),
                      (2, 4),
                      (2, 5),
                      (2, 6)]

    for i in 1:100000
        living_cells = next_population(living_cells)
        # println(living_cells)
        # print_board(living_cells)
        # sleep(0.2)
    end
    println(living_cells)
end
