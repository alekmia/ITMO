package laba_1;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner myScan = new Scanner(System.in);
        int n = myScan.nextInt();
        int minim = 710;
        for (int i = -n / 2; i < n / 2 + (n % 2 == 0 ? 0 : 1); i++) {
            System.out.println(i * minim);
        }
    }
}
