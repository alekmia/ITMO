package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public interface Queue {

    // PRED element != null
    // POST a[tail] = element && n' = n + 1 && immutable(head', tail' - 1)
    void enqueue(final Object element);

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    Object dequeue();

    // PRED  size > 0
    // POST  R = elements[head] && immutable(1, maxSize)
    Object element();

    // PRED true
    // POST R = size && immutable(1, maxSize)
    int size();

    // PRED true
    // POST R = (size == 0) && immutable(1, maxSize)
    boolean isEmpty();

    // PRED true
    // POST n' = 0
    void clear();

    // PRED true
    // POST R = Queue: for i in Queue: Pred(Queue[i]) == true &&
    //      && (forall j1 <= j <= j2 && Pred(arr[i]) == true) exists j1' <= j' <= j2': Queue[j'] == arr[j]
    Queue filter(Predicate<Object> pred);

    // PRED true
    // POST R = Queue: for i in Queue: Queue[i] = func(a[i])
    Queue map(Function<Object, Object> function);
}
