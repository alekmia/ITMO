package game;

public class TwoPlayerGame {
    private final Board board;
    private final Player player1;
    private final Player player2;

    public TwoPlayerGame(Board board, Player player1, Player player2) {
        this.board = board;
        this.player1 = player1;
        this.player2 = player2;
    }

    public int play(boolean log) {
        while (true) {
            try {
                final int result1 = makeMove(player1, 1, log);
                if (result1 != -1) {
                    return result1;
                }
            } catch(Exception e) {
                System.out.println("NO CHEATING ALLOWED");
                return 2;
            }
            try {
                final int result2 = makeMove(player2, 2, log);
                if (result2 != -1) {
                    return result2;
                }
            } catch(Exception e) {
                System.out.println("NO CHEATING ALLOWED");
                return 1;
            }
        }
    }

    private int makeMove(Player player, int no, boolean log) {
        final Move move = player.makeMove(board.getPosition());
        final GameResult result = board.makeMove(move);
        if (log) {
            System.out.println();
            System.out.println("Player: " + no);
            System.out.println(move);
            System.out.println(board);
            System.out.println("Result: " + result);

        }
        switch (result) {
            case WIN:
                return no;
            case LOOSE:
                return 3 - no;
            case DRAW:
                return 0;
            case UNKNOWN:
                return -1;
            default:
                throw new AssertionError("Unknown makeMove result " + result);
        }
    }
}
