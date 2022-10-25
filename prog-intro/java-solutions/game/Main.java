package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {

        Scanner sc = new Scanner(System.in);
//
//        int m = 3;
//        int n = 3;
//        int k = 3;

        System.out.print("What board would you like to play on:\nm = ");
        int n = sc.nextInt();
        System.out.print("n = ");
        int m = sc.nextInt();
        System.out.print("k = ");
        int k = sc.nextInt();
//        int k = 10  ;
//        int m = 41;
//        int n = 41;

        final int result = new TwoPlayerGame(
//                new TicTacToeBoard(),
                new MNKBoard(m, n, k),
                new RandomPlayer(m, n),
//                new RandomPlayer(m, n)
//                new SequentialPlayer(m, n),
//                new SequentialPlayer(m, n)
//                new HumanPlayer(new Scanner(System.in))
                new HumanPlayer(new Scanner(System.in))
//                new CheatingPlayer(m, n, k)
        ).play(true);
        switch (result) {
            case 1:
                System.out.println("First player won");
                break;
            case 2:
                System.out.println("Second player won");
                break;
            case 0:
                System.out.println("Draw");
                break;
            default:
                throw new AssertionError("Unknown result " + result);
        }
    }
}
