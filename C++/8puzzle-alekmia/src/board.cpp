#include "board.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <numeric>
#include <random>

Board Board::create_goal(const unsigned size)
{
    Board goal;
    goal.boardValues.resize(size);
    for (unsigned i = 0; i < size; i++) {
        goal.boardValues[i].resize(size);
    }
    for (unsigned i = 0; i < size; i++) {
        for (unsigned j = 0; j < size; j++) {
            goal.boardValues[i][j] = i * size + j + 1;
        }
    }
    if (size > 0) {
        goal.boardValues[size - 1][size - 1] = 0;
        goal.freeSpace = {size - 1, size - 1};
    }
    return goal;
}

Board Board::create_random(const unsigned size)
{
    Board random_board;
    random_board.boardValues.resize(size);
    for (unsigned i = 0; i < size; i++) {
        random_board.boardValues[i].resize(size);
    }

    std::vector<unsigned> vec;
    vec.resize(size * size);
    std::iota(vec.begin(), vec.end(), 0);
    std::shuffle(vec.begin(), vec.end(), std::mt19937(std::random_device{}()));
    for (unsigned i = 0; i < size; i++) {
        for (unsigned j = 0; j < size; j++) {
            random_board.boardValues[i][j] = vec[i * size + j];
            if (vec[i * size + j] == 0) {
                random_board.freeSpace = {i, j};
            }
        }
    }

    return random_board;
}

Board::Board(const std::vector<std::vector<unsigned>> & data)
    : boardValues(data)
{
    for (unsigned i = 0; i < data.size(); i++) {
        for (unsigned j = 0; j < data.size(); j++) {
            if (data[i][j] == 0) {
                freeSpace = {i, j};
            }
        }
    }
}

std::size_t Board::size() const
{
    return boardValues.size();
}

bool Board::is_goal() const
{
    return manhattan() == 0;
}

unsigned Board::hamming() const
{

    return hammingAndManhattan().first;
}

unsigned Board::manhattan() const
{

    return hammingAndManhattan().second;
}

std::pair<unsigned, unsigned> Board::hammingAndManhattan() const
{
    unsigned hammingCounter = 0;
    unsigned size = this->size();
    int goalX;
    int goalY;
    int goalNumber;
    int manhattanCounter = 0;
    for (unsigned i = 0; i < size; i++) {
        for (unsigned j = 0; j < size; j++) {
            goalNumber = boardValues[i][j];
            if (goalNumber != 0) {
                goalY = (goalNumber % size - 1 + size) % size;
                goalX = (goalNumber - 1) / size;
                manhattanCounter += std::abs(static_cast<int>(goalX - i)) + std::abs(static_cast<int>(goalY - j));
            }
            if (boardValues[i][j] != (i * size + j + 1) % (size * size)) {
                hammingCounter++;
            }
        }
    }
    return {hammingCounter, manhattanCounter};
}

std::string Board::to_string() const
{
    std::string ans;
    for (const auto & i : boardValues) {
        for (const auto & j : i) {
            ans += std::to_string(j) + " ";
        }
        ans += "\n";
    }
    return ans;
}

bool Board::is_solvable() const
{
    int flag = 0;
    int inversionCounter = 0;
    std::vector<unsigned> countingInversions;
    unsigned bsize = size();
    if (bsize < 2) {
        return true;
    }
    countingInversions.resize(bsize * bsize);
    for (unsigned i = 0; i < bsize; i++) {
        for (unsigned j = 0; j < bsize; j++) {
            if (boardValues[i][j] != 0) {
                countingInversions[i * bsize + j + flag] = boardValues[i][j];
            }
            else {
                flag = -1;
            }
        }
    }
    for (unsigned i = 0; i < countingInversions.size() - 2; i++) {
        for (unsigned j = i + 1; j < countingInversions.size() - 1; j++) {
            if (countingInversions[i] > countingInversions[j]) {
                inversionCounter++;
            }
        }
    }
    if (size() % 2 == 0) {
        inversionCounter += 1 + freeSpace.first;
    }
    return inversionCounter % 2 == 0;
}

bool Board::validMove(direction dir) const
{
    if (dir == direction::left && freeSpace.second > 0) {
        return true;
    }
    if (dir == direction::right && freeSpace.second < size() - 1) {
        return true;
    }
    if (dir == direction::up && freeSpace.first > 0) {
        return true;
    }
    if (dir == direction::down && freeSpace.first < size() - 1) {
        return true;
    }
    return false;
}

std::shared_ptr<Board> Board::move(direction dir) const
{
    std::shared_ptr<Board> boardAfterMove = std::make_shared<Board>(*this);

    if (dir == direction::right && validMove(dir)) {
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second] =
                boardAfterMove->boardValues[freeSpace.first][freeSpace.second + 1];
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second + 1] = 0;
        boardAfterMove->freeSpace = {freeSpace.first, freeSpace.second + 1};
    }
    if (dir == direction::left && validMove(dir)) {
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second] =
                boardAfterMove->boardValues[freeSpace.first][freeSpace.second - 1];
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second - 1] = 0;
        boardAfterMove->freeSpace = {freeSpace.first, freeSpace.second - 1};
    }
    if (dir == direction::up && validMove(dir)) {
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second] =
                boardAfterMove->boardValues[freeSpace.first - 1][freeSpace.second];
        boardAfterMove->boardValues[freeSpace.first - 1][freeSpace.second] = 0;
        boardAfterMove->freeSpace = {freeSpace.first - 1, freeSpace.second};
    }
    if (dir == direction::down && validMove(dir)) {
        boardAfterMove->boardValues[freeSpace.first][freeSpace.second] =
                boardAfterMove->boardValues[freeSpace.first + 1][freeSpace.second];
        boardAfterMove->boardValues[freeSpace.first + 1][freeSpace.second] = 0;
        boardAfterMove->freeSpace = {freeSpace.first + 1, freeSpace.second};
    }
    return boardAfterMove;
}