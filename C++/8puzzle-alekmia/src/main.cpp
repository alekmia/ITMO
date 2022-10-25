#include "solver.h"

#include <algorithm>
#include <iostream>

int main()
{
    const auto board = Board::create_random(4);
    const auto solution = Solver::solve(board);
    std::cout << solution.moves() << std::endl;
    for (const auto & move : solution) {
        std::cout << move << std::endl;
    }
}
