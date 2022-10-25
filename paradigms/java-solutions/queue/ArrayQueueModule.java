package queue;

import java.util.HashMap;


// Inv - for i in [head, tail]  a[i] != null
// Immutable(i1, i2) - for i in [i1, i2] a'[i] == a[i]


public class ArrayQueueModule {
    private static Object[] elements = new Object[2];
    private static int head = 0;
    private static int tail = 0;

    // PRED element != null
    // POST a[tail] = element && n' = n + 1 && immutable(head', tail' - 1)
    public static void enqueue(final Object element) {
        ensureBig();
        elements[tail] = element;
        tail = (tail + 1) % elements.length;
    }

    // PRED capacity > 0
    // POST n' >= n && immutable(head, tail)
    public static void ensureBig() {
        if (size() + 1 >= elements.length) {
            Object[] tempArr = new Object[elements.length * 2];
            int pointer = head;
            int i = 0;
            while (pointer != tail) {
                tempArr[i] = elements[pointer];
                pointer = (pointer + 1) % elements.length;
                i++;
            }
            head = 0;
            tail = elements.length - 1;
            elements = tempArr;
        }
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    public static Object dequeue() {
        assert !isEmpty();
        Object x = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        return x;
    }

    // PRED  size > 0
    // POST  R = elements[head] && immutable(1, elements.length)
    public static Object element() {
        return elements[head];
    }

    // PRED true
    // POST R = size && immutable(1, elements.length)
    public static int size() {
        return tail >= head ? tail - head : tail + elements.length - head;
    }

    // PRED true
    // POST R = (size == 0) && immutable(1, elements.length)
    public static boolean isEmpty() {
        return size() == 0;
    }

    // PRED true
    // POST queue is clear (n' = 0)
    public static void clear() {
        head = 0;
        tail = 0;
        elements = new Object[2];
    }


    // PRED element != null
    // POST a[head] = element && n' = n + 1 && immutable(head' + 1, tail')
    public static void push(Object element) {
        assert element != null;
        ensureBig();
        head = (head - 1 + elements.length) % elements.length;
        elements[head] = element;

    }

    // PRED  size > 0
    // POST  R = elements[tail - 1] && immutable(1, elements.length)
    public static Object peek() {
        return elements[tail - 1];
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    public static Object remove() {
        assert !isEmpty();
        tail = (tail - 1) % elements.length;
        Object x = elements[tail];
        elements[tail] = null;
        return x;
    }

    // PRED true
    // POST R = number of times n in arr
    public static int count(Object element) {
        int ans = 0;
        for (int i = head; i != tail; i = (i + 1) % elements.length) {
            if (elements[i] == element) {
                ans++;
            }
        }
        return ans;
    }
}
