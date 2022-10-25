package queue;


// Inv - for i in [head, tail]  a[i] != null
// Immutable(i1, i2) - for i in [i1, i2] a'[i] == a[i]

import java.lang.reflect.Array;

public class ArrayQueue extends AbstractQueue {
    private Object[] elements = new Object[2];
    private int head = 0;
    private int tail = 0;

    // PRED true
    // POST R = ArrayQueue() size = 0
    public ArrayQueue createQueue() {
        return new ArrayQueue();
    }

    // PRED element != null
    // POST a[tail] = element && n' = n + 1 && immutable(head', tail' - 1)
    public void enqueueImpl(final Object element) {
        ensureBig();
        elements[tail] = element;
        tail = (tail + 1) % elements.length;
    }

    // PRED capacity > 0
    // POST n' >= n && immutable(head, tail)
    public void ensureBig() {
        if (size + 1 >= elements.length) {
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
    public Object dequeueImpl() {
        assert !isEmpty();
        Object x = elements[this.head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        return x;
    }

    // PRED  size > 0
    // POST  R = elements[head] && immutable(1, elements.length)
    public Object element() {
        return elements[head];
    }


    // PRED true
    // POST R = (size == 0) && immutable(1, elements.length)
    public boolean isEmpty() {
        return size == 0;
    }

    // PRED element != null
    // POST a[head] = element && n' = n + 1 && immutable(head' + 1, tail')
    public void push(Object element) {
        ensureBig();
//        head = (head - 1 + elements.length) % elements.length;
        head = (head - 1) % elements.length;
        elements[head] = element;
        size++;

    }

    // PRED  size > 0
    // POST  R = elements[tail - 1] && immutable(1, elements.length)
    public Object peek() {
        return elements[tail - 1];
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    public Object remove() {
        assert !isEmpty();
        tail = (tail - 1 + elements.length) % elements.length;
        Object x = elements[tail];
        elements[tail] = null;
        size--;
        return x;
    }

    // PRED true
    // POST R = number of n in arr
    public int count(Object element) {
        int ans = 0;
        for (int i = head; i != tail; i = (i + 1) % elements.length) {
            if (elements[i] == element) {
                ans++;
            }
        }
        return ans;
    }
}


