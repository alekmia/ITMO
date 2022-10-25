//package laba_1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        int a, b, n;
        int ans = 0;
        int i = 0, j = 0;
        Scanner myScan = new Scanner(System.in);
        a = myScan.nextInt();
        b = myScan.nextInt();
        n = myScan.nextInt();
        while(j + b < n || i + a < n) {
            i += b - a;
            ans++;
            if(j + b < n) {
                j += b - a;
                ans++;
            }
        }
        System.out.println(ans);
    }
}
