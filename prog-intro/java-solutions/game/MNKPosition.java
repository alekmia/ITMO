package game;

import java.util.Arrays;
import java.util.Map;

public class MNKPosition implements Position{
    int m, n, k;
    private final Cell[][] field;
    private Cell turn;

    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0"
    );

    public MNKPosition(Cell[][] field, int m, int n, int k, Cell turn) {
        this.m = m;
        this.n = n;
        this.k = k;
        this.field = field; //was [3][3]

        this.turn = turn;
    }

    @Override
    public Cell getTurn() {
        return turn;
    }

    @Override
    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < m
                && 0 <= move.getCol() && move.getCol() < n
                && field[move.getRow()][move.getCol()] == Cell.E
                && turn == move.getValue();
    }

    @Override
    public Cell getCell(int row, int column) {
        return field[row][column];
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
