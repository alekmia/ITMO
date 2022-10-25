package game;

import java.util.Arrays;

public class TournamentGame {
    private Board board;
    private int amount;
    private final Player[] contestants;
    private int[] scores;
    private int n, m, k;

    public TournamentGame(int m, int k, Board board, Integer playerAmount, Player[]contendants, int[] scores) {
        this.n = n;
        this.m = m;
        this.k = k;
        this.board = board;
        this.amount = playerAmount;
        this.contestants = contendants;
        this.scores = scores;
    }

    public int play(boolean log) {
        boolean flag = true;
        boolean[] bannedPlayers = new boolean[amount];
        for(int i = 0; i < amount; i++) {
            bannedPlayers[i] = false;
        }
        int ans = -1;
        for(int c1 = 0; c1 < amount; c1++) {
            for(int c2 = 0; c2 < amount; c2++) {
                ans = -1;
                board = new HexBoard(m, k);
                if(bannedPlayers[c1] && !bannedPlayers[c2]) {
                        scores[c2] += 3;
                } else if(bannedPlayers[c2] && !bannedPlayers[c1]) {
                    scores[c1] += 3;
                } else if(c1 != c2) {
                    int[] playingRightNow = {c1, c2};
                    System.out.println("NOW PLAYING: PLAYER " + (c1 + 1)  + " AS 'X' "+ " AGAINST PLAYER " + (c2 + 1) + " AS 'O'");
                    while (flag != false) {


                        try {
                            if (flag) {
                                final int result1 = makeMove(contestants[c1], 1, log);
                                if (result1 != -1) {
                                    ans = result1;
                                    flag = false;
                                }
                            }
                        } catch (Exception e) {
                            System.out.println("NO CHEATING ALLOWED AND YOU ARE DISQUALIFIED");
                            scores[c1] = -10000000;
                            bannedPlayers[c1] = true;
                            ans = 2;
                            flag = false;
                        }


                        try {
                            if (flag) {
                                final int result2 = makeMove(contestants[c2], 2, log);
                                if (result2 != -1) {
                                    ans = result2;
                                    flag = false;
                                }
                            }
                        } catch (Exception e) {
                            System.out.println("NO CHEATING ALLOWED AND YOU ARE DISQUALIFIED");
                            scores[c2] = -10000000;
                            bannedPlayers[c2] = true;
                            ans = 1;
                            flag = false;
                        }
                    }

                    switch (ans) {
                        case 1:
                            System.out.println("Player #" + (c1 + 1) +  " won");
                            System.out.println("Player #" + (c1 + 1) +  " gains 3 points");
                            scores[c1] += 3;
                            break;
                        case 2:
                            System.out.println("Player #" + (c2 + 1) +  " won");
                            System.out.println("Player #" + (c2 + 1) +  " gains 3 points");
                            scores[c2] += 3;
                            break;
                        case 0:
                            System.out.println("Draw");
                            System.out.println("Both players gain a point");
                            scores[c1] += 1;
                            scores[c2] += 1;
                            break;
                        default:
                            throw new AssertionError("Unknown result " + ans);
                    }
                }
                flag = true;
            }
        }
        System.out.println("\n\n\n\n      FINAL SCOREBOARD: \n");
        for(int i = 0; i < amount; i++) {
            System.out.println("Player #" + (i + 1) + " has " + scores[i] + " points" + (scores[i] < 0 ? " as he has cheated!" : "!"));
        }
        Pair[] sortedPlayers = new Pair[amount];
        for(int i = 0; i < amount; i++){
            sortedPlayers[i] = new Pair(scores[i], i);
        }
        /*for(int i = 0; i < amount; i++){
           System.out.println(sortedPlayers[i].first + " " + sortedPlayers[i].second);
        }*/
        Arrays.sort(sortedPlayers, 0, amount);
        System.out.println("\n\n\n\n     AAAAAND OUR WINNER IIIIIIS: \n");
        System.out.println("Winner: Player #" + (sortedPlayers[amount - 1].second + 1));
        System.out.println("Close second place: Player #" + (sortedPlayers[amount - 2].second + 1));
        System.out.println("Close third place: Player #" + (sortedPlayers[amount - 3].second + 1));

        return 1;
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

class Pair implements Comparable<Pair>{
    public int first;
    public int second;

    public Pair(int first, int second) {
        this.first = first;
        this.second = second;
    }
    public int compareTo(Pair o) {
        return this.first - o.first;
    }
}