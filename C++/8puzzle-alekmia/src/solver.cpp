#include "solver.h"

#include <algorithm>
#include <set>

struct BoardCharacteristics
{
    std::shared_ptr<Board> board;
    int hamming;
    int manhattan;
    int depth;

    BoardCharacteristics(const std::shared_ptr<Board> & board, int hamming, int manhattan, int depth)
        : board(board)
        , hamming(hamming)
        , manhattan(manhattan)
        , depth(depth)
    {
    }
};

struct Compare
{
    float func(const BoardCharacteristics & a) const
    {
        return 2.5 * a.hamming + 10 * a.manhattan + 2 * a.depth;
    }

    bool operator()(const BoardCharacteristics & a,
                    const BoardCharacteristics & b) const
    {
        float euristicLeft = func(a);
        float euristicRight = func(b);
        if (euristicLeft != euristicRight) {
            return euristicLeft < euristicRight;
        }
        else {
            return *a.board < *b.board;
        }
    }
};

struct CompareBoards
{
    bool operator()(const std::shared_ptr<Board> & a, const std::shared_ptr<Board> & b) const
    {
        return *a < *b;
    }
};

void place(std::shared_ptr<Board> lastBoard, const std::shared_ptr<Board> & board, const std::set<std::shared_ptr<Board>, CompareBoards> & doneMoves, std::set<BoardCharacteristics, Compare> & possibleChoices, int layer)
{
    if (doneMoves.find(board) == doneMoves.end()) {
        auto pp = find_if(possibleChoices.cbegin(), possibleChoices.cend(), [&lastBoard](const BoardCharacteristics & p) {
            return p.board == lastBoard;
        });
        if (pp == possibleChoices.end()) {
            auto hamAndManh = board->hammingAndManhattan();
            BoardCharacteristics forInsertion(board, hamAndManh.first, hamAndManh.second, layer + 1);
            possibleChoices.insert(forInsertion);
        }
    }
}

Solver::Solution Solver::solve(const Board & board)
{
    Solution answer;
    std::shared_ptr<Board> boardNow = std::make_shared<Board>(board);
    std::set<BoardCharacteristics, Compare> possibleChoices;
    std::set<std::shared_ptr<Board>, CompareBoards> doneMoves;

    int layer = 1;
    if (board.is_solvable()) {
        answer.m_moves.push_back(board);
        if (!board.is_goal()) {

            Board initial = *boardNow;
            std::shared_ptr<Board> nextMove = nullptr;
            while (!boardNow->is_goal()) {
                doneMoves.insert(boardNow);

                std::shared_ptr<Board> rightMove = boardNow->move(Board::direction::right);
                rightMove->parent = boardNow;

                std::shared_ptr<Board> leftMove = boardNow->move(Board::direction::left);
                leftMove->parent = boardNow;

                std::shared_ptr<Board> upMove = boardNow->move(Board::direction::up);
                upMove->parent = boardNow;

                std::shared_ptr<Board> downMove = boardNow->move(Board::direction::down);
                downMove->parent = boardNow;

                if (boardNow->validMove(Board::direction::right)) {
                    place(boardNow, rightMove, doneMoves, possibleChoices, layer);
                }
                if (boardNow->validMove(Board::direction::left)) {
                    place(boardNow, leftMove, doneMoves, possibleChoices, layer);
                }
                if (boardNow->validMove(Board::direction::up)) {
                    place(boardNow, upMove, doneMoves, possibleChoices, layer);
                }
                if (boardNow->validMove(Board::direction::down)) {
                    place(boardNow, downMove, doneMoves, possibleChoices, layer);
                }

                int saveLayer = (*possibleChoices.begin()).depth;
                nextMove = (*possibleChoices.begin()).board;
                boardNow = (possibleChoices.extract(possibleChoices.begin()).value()).board;
                layer = saveLayer + 1;
            }
            if (!initial.is_goal()) {
                std::shared_ptr<Board> par = nextMove;
                answer.m_moves.clear();
                while (par->parent != nullptr) {
                    answer.m_moves.emplace_back(*par);
                    auto otto = par->parent;
                    par = otto;
                }
                answer.m_moves.emplace_back(*par);
            }
            std::reverse(answer.m_moves.begin(), answer.m_moves.end());
        }
    }
    return answer;
}