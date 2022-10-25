//package laba_1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class Main {
    public static void main(String[] args) {
        Scanner myScan = new Scanner(System.in);
        int n, x, y, h;
        int xl = Integer.MAX_VALUE, xr = Integer.MIN_VALUE, yl = Integer.MAX_VALUE, yr = Integer.MIN_VALUE;
        n = myScan.nextInt();
        for(int p = 0; p < n; p++)
        {
            x = myScan.nextInt();
            y = myScan.nextInt();
            h = myScan.nextInt();
            xl = min(xl, x - h);
            xr = max(xr, x + h);
            yl = min(yl, y - h);
            yr = max(yr, y + h);
        }
        int ans1, ans2, ans3;
        int ost = max(xr - xl, yr - yl) % 2 == 0 ? 0 : 1;
        ans1 = max(xr - xl, yr - yl) / 2 + ost;
        ans2 = (xl + xr) / 2;
        ans3 = (yl + yr) / 2;
        System.out.println(ans2 + " " + ans3 + " " + ans1);
    }
}
