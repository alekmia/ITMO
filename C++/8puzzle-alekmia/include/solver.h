#pragma once

#include "board.h"

#include <set>

class Solver
{
private:
    class Solution
    {
    public:
        std::size_t moves() const
        {
            if (!m_moves.empty()) {
                return m_moves.size() - 1;
            }
            return m_moves.size();
        }

        using const_iterator = std::vector<Board>::const_iterator;

        const_iterator begin() const { return m_moves.begin(); }

        const_iterator end() const { return m_moves.end(); }

    private:
        friend class Solver;

        std::vector<Board> m_moves;
    };

public:
    static Solution solve(const Board & initial);
};
