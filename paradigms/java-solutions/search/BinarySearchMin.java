package search;

public class BinarySearchMin {
    // PRED args.length > 0 && args.length != +inf   &&   foreach q: args[q] is int
    // POST prints out answer
    public static void main(final String[] args) {
        // args.length > 0 && args.length != +inf && args[q] is int
        final int size = args.length;
        // args.length > 0 && args.length != +inf && args[q] is int
        final int[] arr = new int[size];
        // arr.size() > 0 && args.length > 0 && args.length != +inf
        int i = 0;
        // arr.size() > 0 && args.length > 0 && args.length != +inf
        while(i < size) {
            // i < (size = args.length) && args.length != +inf
            arr[i] = Integer.parseInt(args[i]);
            i++;
            // i <= size && args.length != +inf
        }

        //we will leave the loop because size - i lessens by 1 every while iteration and size != +inf.
        //arr will fill with ints and their amount will be equal to size (from previous line)

        // SAN
        System.out.println(binarySearch1(arr));
        //System.out.println(binarySearch2(arr,0,size));
    }


    // Sorted as needed (SAN) = exists x0: each y < x <= x0 <=> arr[x] < arr[y]  &&  each x,y >= x0 + 1: x > y <=> arr[x] > arr[y]


    //PRED SAN
    //binarySearch1(...)
    //POST SAN && if exists arr[R-1]: arr[R-1] >= arr[R] && if exists arr[R+1]: arr[R+1] > arr[R] (here l + 1 is r)
    public static int binarySearch1(final int[] arr) {
        //SAN
        int l = 0;
        //SAN && l = 0
        int r = arr.length;
        //SAN && l = 0 && r = arr.length() && arr[r] == +inf && arr[l-1] == +inf
        // r - l > 0
        //INV: if exists arr[l'-1]: arr[l'] <= arr[l' - 1] && if exists arr[r']: arr[r'] > arr[r' - 1]
        while (1 != r - l) {
            //INV && SAN && l = 0 && r = arr.length() && arr[r] == +inf && arr[l-1] == +inf
            final int m1 = (l + r) / 2;
            //INV && SAN && l = 0 && r = arr.length() && arr[r] == +inf && arr[l-1] == +inf
            if (arr[m1] > arr[m1 - 1]) {
                //INV && SAN && arr[m1'] > arr[m1' - 1]
                r = m1;
                //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1]
            } else {
                //INV && arr[m1'] <= arr[m1' - 1]
                l = m1;
                //arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1]
            }
            // r' - l' < r - l
        }

        // the loop will break because r - l each iteration of while decreases by (r-l)/2:
        // (r - l) - (m1 - l) = r - m1 = r - (r + l) / 2 = (r - l) / 2
        // or
        // (r - l) - (r - m1) = m1 - l = (r + l) / 2 - l = (r - l) / 2
        //  if the loop is infinite, then r - l >= 2 always => (r - l)/2 >= 1 meaning that r - l = infinity
        // r - l != 0 cause (r - l) / 2 = r - l meaning r - l = 1, which means we had to already have left the while


        //arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1] && r' = l' + 1
        //arr[l'] <= arr[l' - 1] && arr[l'+ 1] > arr[l']
        // SAN && if exists arr[l'-1]: arr[l'-1] >= arr[l'] && if exists arr[l'+1]: arr[l'+1] > arr[l']
        return arr[l];
    }

    //PRED SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1]                                                         (0)
    //binarySearch2(...)
    //POST SAN && if exists arr[l-1] arr[l-1] >= arr[l] && if exists arr[l+1] arr[l+1] > arr[l]
    public static int binarySearch2(final int[] arr, final int l, final int r) {
        //SAN && if exists arr[l'-1] arr[l'] <= arr[l' - 1] && if exists arr[r'] arr[r'] > arr[r' - 1]
        final int m1 = (l + r) / 2;
        //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1]
        if (r - l != 1) {
            //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1] && r - l != 1
            if (arr[m1] > arr[m1-1]) {
                //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1] && arr[m1] > arr[m1-1] && r - l != 1             (1)
                return binarySearch2(arr, l, m1);
            } else {
                //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1] && arr[m1] <= arr[m1-1] && r - l != 1            (2)
                return binarySearch2(arr, m1, r);
            }
        } else {
            //SAN && arr[l'] <= arr[l' - 1] && arr[r'] > arr[r' - 1] && r' = l' + 1  =>
            //SAN && arr[l'] <= arr[l' - 1] && arr[l' + 1] > arr[l'] && r = l + 1
            return arr[l];
        }

    }

    // why (1) => (0)? arr[l'] <= arr[l' - 1]  && arr[m1] > arr[m1-1] from (1) is exactly (0) cause r = m1;
    // why (2) => (0)? arr[r'] > arr[r' - 1]  && arr[m1] <= arr[m1-1] from (2) is exactly (0) cause l = m1;


    // the recursion will break because r - l each iteration of while decreases by (r-l)/2:
    // (r - l) - (m1 - l) = r - m1 = r - (r + l) / 2 = (r - l) / 2
    // or
    // (r - l) - (r - m1) = m1 - l = (r + l) / 2 - l = (r - l) / 2
    // if the recursion is infinite, then r - l >= 2 always => (r - l)/2 >= 1 meaning that r - l = infinity
    // r - l != 0 cause (r - l) / 2 = r - l meaning r - l = 1, which means we had to already have left the first if
}
