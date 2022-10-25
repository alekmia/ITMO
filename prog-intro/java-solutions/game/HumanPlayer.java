package game;

import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner in;

    public HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        System.out.println();
        System.out.println("Current position");
        System.out.println(position);
        System.out.println("Enter your move for " + position.getTurn());
        while (true) {
            try {
                Scanner lineScanner = new Scanner(in.nextLine());
                int x = Integer.parseInt(lineScanner.next()) - 1;
                int y = Integer.parseInt(lineScanner.next()) - 1;
                final Move move = new Move(x, y, position.getTurn());
                if (position.isValid(move)) {
                    return move;
                }

            } catch (Exception e) {
            }
            System.out.println();
            System.out.println("Pls try again: ");
        }
    }
}