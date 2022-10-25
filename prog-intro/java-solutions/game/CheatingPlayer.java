package game;

import java.util.Scanner;

public class CheatingPlayer implements Player {
    int m, n;
    public CheatingPlayer(int m, int n, int k) {
        this.m = m;
        this.n = n;
    }

    @Override
    public Move makeMove(Position position) {
        final MNKBoard board = (MNKBoard) position;
        Move first = null;
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < m; c++) {
                final Move move = new Move(r, c, position.getTurn());
                if (position.isValid(move)) {
                    if (first == null) {
                        first = move;
                    } else {
                        board.makeMove(move);
                    }
                }
            }
        }

        return first;
    }
}
