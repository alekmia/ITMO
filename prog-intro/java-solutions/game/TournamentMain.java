package game;

import java.util.Scanner;

public class TournamentMain {
    public static void main(String[] args) {
        boolean flag = true;
        int n = 0;
        int m = 0;
        int k = 0;
        while(flag) {
            try {
                Scanner sc = new Scanner(System.in);
                System.out.print("What board would you like to play on:\nm = ");
                m = sc.nextInt();
                flag = false;
            } catch (Exception e) {
                System.out.println("that's an incorrect input... try again");
            }
        }
        flag = true;
        while(flag) {
            try {
                Scanner sc = new Scanner(System.in);
                System.out.print("k = ");
                k = sc.nextInt();
                flag = false;
            } catch (Exception e) {
                System.out.println("that's an incorrect input... try again");
            }
        }

        int playerAmount = 6;
        Player[] contestants = new Player[]{
                new RandomPlayer(m, m),
                new RandomPlayer(m, m),
                new RandomPlayer(m, m),
                new RandomPlayer(m, m),
                new RandomPlayer(m, m),
                new RandomPlayer(m, m)
               // new CheatingPlayer(m, m, k)
               // new HumanPlayer(new Scanner(System.in))
        };
        int[] scores = new int[playerAmount];

//        int m = 3;
//        int n = 3;
//        int k = 3;

        final int result = new TournamentGame(m, k,
                new HexBoard(m, k), playerAmount, contestants, scores
        ).play(true);
    }
}
