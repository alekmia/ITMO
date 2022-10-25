#pragma once

#include <memory>
#include <string>
#include <vector>

class Board
{
private:
    enum class direction
    {
        nothing,
        left,
        right,
        up,
        down
    };

    friend class Solver;
    std::vector<std::vector<unsigned>> boardValues;
    std::pair<unsigned, unsigned> freeSpace;
    std::shared_ptr<Board> parent = nullptr;

    bool validMove(direction dir) const;

    std::shared_ptr<Board> move(direction dir) const;

public:
    static Board create_goal(unsigned size);

    static Board create_random(unsigned size);

    Board() = default;

    explicit Board(const std::vector<std::vector<unsigned>> & data);

    std::size_t size() const;

    bool is_goal() const;

    unsigned hamming() const;

    unsigned manhattan() const;

    std::pair<unsigned, unsigned> hammingAndManhattan() const;

    std::string to_string() const;

    bool is_solvable() const;

    std::vector<unsigned> & operator[](const unsigned rhs)
    {
        return boardValues[rhs];
    }

    const std::vector<unsigned> & operator[](const unsigned rhs) const
    {
        return boardValues[rhs];
    }

    friend bool operator==(const Board & lhs, const Board & rhs)
    {
        return lhs.boardValues == rhs.boardValues;
    }

    friend bool operator!=(const Board & lhs, const Board & rhs)
    {
        return !(lhs == rhs);
    }

    friend std::ostream & operator<<(std::ostream & out, const Board & board)
    {
        return out << board.to_string();
    }

    friend bool operator<(const Board & lhs, const Board & rhs)
    {
        return lhs.boardValues < rhs.boardValues;
    }
};
