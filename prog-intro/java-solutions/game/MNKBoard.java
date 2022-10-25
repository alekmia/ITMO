package game;

import java.util.Arrays;
import java.util.Map;
import java.lang.*;

public class MNKBoard implements Board{
    int m, n, k;

    protected static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0"
    );

    protected final Cell[][] field;
    protected Cell turn;

    public MNKBoard(int m, int n, int k) {
        this.m = m;
        this.n = n;
        this.k = k;
        field = new Cell[n][m]; //was [3][3]
        for (Cell[] row : field) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
    }

    @Override
    public Position getPosition() {
        return new MNKPosition(field, m, n, k, turn);
    }

    @Override
    public GameResult makeMove(Move move) {
        if (!this.getPosition().isValid(move)) {
            return GameResult.LOOSE;
        }

        field[move.getRow()][move.getCol()] = move.getValue();
        if (checkWin(move.getRow(), move.getCol())) {
            return GameResult.WIN;
        }

        if (checkDraw()) {
            return GameResult.DRAW;
        }

        turn = turn == Cell.X ? Cell.O : Cell.X;
        return GameResult.UNKNOWN;
    }

    private boolean checkDraw() {
        int count = 0;
        for (int r = 0; r < m; r++) { // was 3
            for (int c = 0; c < n; c++) {  // was 3
                if (field[r][c] == Cell.E) {
                    count++;
                }
            }
        }
        if (count == 0) { //was 3
            return true;
        }
        return false;
    }

    private boolean checkWin(int x, int y) {

        if (checkLine(x, y, "vertical") || checkLine(x, y, "horizontal"))
            return true;

        if(checkDiag(x, y))
            return true;
        return false;
    }

    public boolean checkLine(int x, int y, String way) {
        int count = 0;
        if(way.equals("vertical")) {
            for (int i = y - k; i < y + k; i++) {
                while (i >= 0 && i < n && field[x][i] == turn) {
                    count++;
                    i++;
                }
                if (count >= k) {
                    return true;
                }
                count = 0;
            }
        }
        else {
            for (int i = x - k; i < x + k; i++) {
                while (i >= 0 && i < m && field[i][y] == turn) {
                    count++;
                    i++;
                }
                if (count >= k) {
                    return true;
                }
                count = 0;
            }
        }
        return false;
    }

    public boolean checkDiag(int x, int y)
    {
        int count = 0;
        for(int i = x - k, j = y - k; i < x + k && j < y + k; i++, j++) {
            while(i >= 0 && j >= 0 && i < m && j < n && field[i][j] == turn) {
                count++;
                i++;
                j++;
            }
            if(count >= k)
            {
                return true;
            }
            count = 0;
        }

        count = 0;
        for(int i = x - k, j = y + k; i < x + k && j > y - k; i++, j--) {
            while (i >= 0 && j >= 0 && i < m && j < n && field[i][j] == turn) {
                count++;
                i++;
                j--;
            }
            if (count >= k) {
                return true;
            }
            count = 0;
        }
        return false;
    }

    public int getDigitCount(int n) {
        int ans = 0;
        while(n > 0) {
            ans++;
            n /= 10;
        }
        return ans;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("___|");
        for(int i = 0; i < n; i++) {
            for (int j = 0; j < 4 - getDigitCount(i + 1); j++)
            {
                sb.append("_");
            }
            sb.append(Integer.toString(i + 1));
            sb.append("|");
        }
        sb.append(System.lineSeparator());
        //(" 123").append(System.lineSeparator());
        for (int r = 0; r < n; r++) {
            for (int j = 0; j < 3 - getDigitCount(r + 1); j++)
            {
                sb.append("_");
            }
            sb.append(r + 1);
            sb.append("|");
            for (Cell cell : field[r]) {
                sb.append("  ");
                sb.append(CELL_TO_STRING.get(cell));
                sb.append(" |");
            }
            sb.append(System.lineSeparator());
        }
        sb.setLength(sb.length() - System.lineSeparator().length());
        return sb.toString();
    }
}
