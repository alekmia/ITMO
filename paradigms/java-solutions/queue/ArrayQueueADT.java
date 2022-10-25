package queue;

import java.util.HashMap;


// Inv - for i in [head, tail]  a[i] != null
// Immutable(i1, i2) - for i in [i1, i2] a'[i] == a[i]


public class ArrayQueueADT {
    private Object[] elements = new Object[2];
    private int head = 0;
    private int tail = 0;
    private int maxSize = 2;

    // PRED element != null
    // POST a[tail] = element && n' = n + 1 && immutable(head', tail' - 1)
    public static void enqueue(final ArrayQueueADT queue, final Object element) {
        ensureBig(queue);
        queue.elements[queue.tail] = element;
        queue.tail = (queue.tail + 1) % queue.maxSize;
    }

    // PRED true
    // POST n' >= n && immutable(head, tail)
    public static void ensureBig(final ArrayQueueADT queue) {
        if (queue.size(queue) + 1 >= queue.maxSize) {
            Object[] tempArr = new Object[queue.maxSize * 2];
            int pointer = queue.head;
            int i = 0;
            while (pointer != queue.tail) {
                tempArr[i] = queue.elements[pointer];
                pointer = (pointer + 1) % queue.maxSize;
                i++;
            }
            queue.head = 0;
            queue.tail = queue.maxSize - 1;
            queue.elements = tempArr;
            queue.maxSize *= 2;
        }
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    public static Object dequeue(final ArrayQueueADT queue) {
        assert !isEmpty(queue);
        Object x = queue.elements[queue.head];
        queue.elements[queue.head] = null;
        queue.head = (queue.head + 1) % queue.maxSize;
        return x;
    }

    // PRED  size > 0
    // POST  R = elements[head] && immutable(1, maxSize)
    public static Object element(final ArrayQueueADT queue) {
        return queue.elements[queue.head];
    }

    // PRED true
    // POST R = size && immutable(1, maxSize)
    public static int size(final ArrayQueueADT queue) {
        return queue.tail >= queue.head ? queue.tail - queue.head : queue.tail + queue.elements.length - queue.head;
    }

    // PRED true
    // POST R = (size == 0) && immutable(1, maxSize)
    public static boolean isEmpty(final ArrayQueueADT queue) {
        return queue.size(queue) == 0;
    }

    // PRED true
    // POST n' = 0
    public static void clear(final ArrayQueueADT queue) {
        queue.head = 0;
        queue.tail = 0;
        queue.elements = new Object[2];
        queue.maxSize = 2;
    }

    // PRED element != null
    // POST a[head] = element && n' = n + 1 && immutable(head' + 1, tail')
    public static void push(final ArrayQueueADT queue, Object element) {
        ensureBig(queue);
        queue.head = (queue.head - 1 + queue.maxSize) % queue.maxSize;
        queue.elements[queue.head] = element;
    }

    // PRED  size > 0
    // POST  R = elements[tail - 1] && immutable(1, maxSize)
    public static Object peek(final ArrayQueueADT queue) {
        return queue.elements[queue.tail - 1];
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    public static Object remove(final ArrayQueueADT queue) {
        assert !isEmpty(queue);
        queue.tail = (queue.tail - 1 + queue.maxSize) % queue.maxSize;
        Object x = queue.elements[queue.tail];
        queue.elements[queue.tail] = null;
        return x;
    }

    // PRED true
    // POST R = number of times n in arr
    public static int count(final ArrayQueueADT queue, Object element) {
        int ans = 0;
        for (int i = queue.head; i != queue.tail; i = (i + 1) % queue.elements.length) {
            if (queue.elements[i] == element) {
                ans++;
            }
        }
        return ans;
    }
}
