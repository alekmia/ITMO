//package laba_1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner myScan = new Scanner(System.in);
        int n = myScan.nextInt();
        String temp;
        int[][] bracelet = new int[n][n];
        for(int i = 0; i < n; i++) {
            temp = myScan.next();
            for (int j = 0; j < n; j++) {
                bracelet[i][j] = temp.charAt(j) - '0';
            }
        }

        for(int i = 0; i < n; i++) {
            for(int j = i + 1; j < n; j++) {
                if(bracelet[i][j] != 0) {
                    for(int k = 1; k < n; k++) {
                        bracelet[i][k] = (bracelet[i][k] - bracelet[j][k] + 10) % 10;
                    }
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                System.out.print(bracelet[i][j]);
            }
            System.out.println();
        }

    }
}
