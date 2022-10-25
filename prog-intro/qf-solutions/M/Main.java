//package laba_1;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner myScan = new Scanner(System.in);
        int t = myScan.nextInt();
        int v;

        for(int p = 0; p < t; p++) {
            int n = myScan.nextInt();
            int[] arr = new int[n];
            int ans = 0;
            for(int j = 0; j < n; j++) {
                arr[j] = myScan.nextInt();
            }
            HashMap<Integer, Integer> C = new HashMap<>();
            C.put(arr[n-1], 1);
            for(int j = n - 2; j > 0; j--) {
                for(int i = 0; i < j; i++) {
                    v = 2 * arr[j] - arr[i];
                    if(C.containsKey(v)) {
                        ans += C.get(v);
                    }
                }
/*                if(C.containsKey(arr[j])) {
                    C.put(arr[j], C.get(arr[j]) + 1);
                }
                else {
                    C.put(arr[j], 1);
                }*/
                C.put(arr[j], C.getOrDefault(arr[j], 0) + 1);
            }
            System.out.println(ans);
        }


    }
}
