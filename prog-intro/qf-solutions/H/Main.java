//package laba_1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner myScan = new Scanner(System.in);
        int n = myScan.nextInt();
        int sum = 0;
        int maxim = 0;
        int [] transactions = new int[n];
        int[] offset = new int[n];
        HashMap<Integer, Integer> answers = new HashMap<>();
        for(int i = 0; i < n; i++) {
            transactions[i] = myScan.nextInt();
            sum += transactions[i];
            maxim = Math.max(maxim, transactions[i]);
            offset[i] = (i != 0 ? offset[i - 1] + transactions[i - 1]: 0) ;
        }
        int[] f = new int[sum + 1];
        int counter = 1;
        for(int i = 0; i < n; i++){
            int temp = transactions[i];
            while(temp > 0) {
                f[counter] = i + 1;
                counter++;
                temp--;
            }
        }
        int q = myScan.nextInt();
        int[] queries = new int[q];
        for(int i = 0; i < q; i++) {
            queries[i] = myScan.nextInt();
        }

        for(int r = 0; r < q; r++) {
            if(maxim > queries[r]) {
                System.out.println("Impossible");
            }
            else if(answers.containsKey(queries[r])) {
                System.out.println(answers.get(queries[r]));
            }
            else {
                int answer = 0;
                int b = 0;
                for(;offset[b] + queries[r] < sum; answer++){
                    b = f[offset[b] + queries[r] + 1] - 1;
                }
                if(b != n)
                    answer++;
                System.out.println(answer);
                answers.put(queries[r], answer);
            }

        }
    }
}
