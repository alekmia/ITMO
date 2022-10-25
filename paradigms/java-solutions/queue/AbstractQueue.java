package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size;

    abstract Queue createQueue();

    public Queue fill(Predicate<Object> pred, Function<Object, Object> function) {
        Queue ret = createQueue();
        Object data;
        for(int i = 0; i < size(); i++) {
           data = dequeue();
           enqueue(data);
           if(pred.test(data)) {
               ret.enqueue(function.apply(data));
           }
        }
        return ret;
    }

    public Queue filter(Predicate<Object> predicate) {
        return fill(predicate, Function.identity());
    }

    public Queue map(Function<Object, Object> function) {
        return fill(x -> true, function);
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public void clear() {
        while(size != 0)
            dequeue();
    }

    public Object dequeue() {
        assert size > 0;
        Object returnValue = dequeueImpl();
        size--;
        return returnValue;
    }

    public void enqueue(final Object element) {
        enqueueImpl(element);
        size++;
    }

    protected abstract Object dequeueImpl();

    protected abstract void enqueueImpl(final Object element);
}
