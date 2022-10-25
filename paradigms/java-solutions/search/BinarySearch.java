package search;

public class BinarySearch {
    //args.length > 0 && args.length != +inf   &&   foreach q: args[q] is int
    public static void main(String[] args) {
        // args.length > 0 && args.length != +inf && args[q] is int
        int size = args.length;
        // args.length > 0 && args.length != +inf && args[q] is int
        int x = Integer.parseInt(args[0]);
        // args.length > 0 && args.length != +inf && args[q] is int
        int[] arr = new int[size - 1];
        // args.length > 0 && args.length != +inf && args[q] is int && arr.size() > 0
        int i = 1;
        // args.length > 0 && args.length != +inf && args[q] is int && arr.size() > 0
        while(i < size) {
            // i < (size = args.length) && args.length != +inf  && SAN
            arr[i - 1] = Integer.parseInt(args[i]);
            // i < (size = args.length) && args.length != +inf && SAN
            i++;
            // i <= (size = args.length) && args.length != +inf && SAN
        }

        //we will leave the loop because size - i lessens by 1 every while iteration and size != +inf.
        //arr will fill with ints and their amount will be equal to size (from previous line)

        // SAN && arr.size < 0
        System.out.println(binarySearch1(arr, x));
        //System.out.println(binarySearch2(arr, x, -1,size - 1));
    }


    // Sorted as needed (SAN) - x > y = > arr[x] <= arr[y]


    //Pred SAN
    //binarySearch1(...)
    //Post l + 1 = r && a[l] > a[r]  && a[l] > x && a[r] <= x
    public static int binarySearch1(int[] arr, int x) {
        // SAN
        int l = -1, r = arr.length;
        // SAN && arr[-1] = +inf && arr[r] = -inf
        //Invariant (INV) l < r a[l] > a[r] && a[l] > m && a[r] <= m
        while (l + 1 < r) {
            // SAN && l + 1 < r && arr[-1] = +inf && arr[r] = -inf
            int m = (l + r) / 2;
            // SAN && l + 1 < r && arr[-1] = +inf && arr[r] = -inf
            if (x < arr[m]) {
                //SAN && x < arr[m]
                l = m;
            } else {
                // SAN && x >= arr[m]
                r = m;
                // because inv is true: SAN && x >= arr[r'] <- with l' inv stays the same
            }
            // INV
        }

        // the loop will break because r - l each iteration of while decreases by (r-l)/2:
        // (r - l) - (m1 - l) = r - m1 = r - (r + l) / 2 = (r - l) / 2
        // or
        // (r - l) - (r - m1) = m1 - l = (r + l) / 2 - l = (r - l) / 2
        //  if the loop is infinite, then r - l >= 2 always => (r - l)/2 >= 1 meaning that r - l = infinity
        // r - l != 0 cause (r - l) / 2 = r - l meaning r - l = 1, which means we had to already have left the while

        // l + 1 = r && a[l] > a[r]  && a[l] > x && a[r] <= x
        return r;
    }

    //Pred SAN && l < r a[l] > a[r] && a[l] > m && a[r] <= m
    //binarySearch1(...)
    //Post l + 1 = r && a[l] > a[r]  && a[l] > x && a[r] <= x
    public static int binarySearch2(int[] arr, int x, int l, int r) {
        // SAN && arr[-1] = +inf && arr[r] = -inf
        int m = (l + r) / 2;
        // SAN && arr[-1] = +inf && arr[r] = -inf
        if (l + 1 < r) {
            // l + 1 < r && a[l] > a[r] && a[l] > x && a[r] <= x
            if (x < arr[m]) {
                // l + 1 < r && a[l] > a[r] && a[l] > x && a[r] <= x && x < arr[m]
                return binarySearch2(arr, x, m, r);
            } else {
                // l + 1 < r && a[l] > a[r] && a[l] > x && a[r] <= x && x >= arr[m]
                return binarySearch2(arr, x, l, m);
            }
        } else {
            // l + 1 = r && a[l] > a[r] && a[l] > x && a[r] <= x
            return l + 1;
        }


        // the recursion will break because r - l each iteration of while decreases by (r-l)/2:
        // (r - l) - (m1 - l) = r - m1 = r - (r + l) / 2 = (r - l) / 2
        // or
        // (r - l) - (r - m1) = m1 - l = (r + l) / 2 - l = (r - l) / 2
        // if the recursion is infinite, then r - l >= 2 always => (r - l)/2 >= 1 meaning that r - l = infinity
        // r - l != 0 cause (r - l) / 2 = r - l meaning r - l = 1, which means we had to already have left the first if

    }

}
