package queue;

public class LinkedQueue extends AbstractQueue {
    private static class Node {
        private final Object data;
        private Node next;

        public Node(Object data, Node next) {
            this.data = data;
            this.next = next;
        }
    }

    private Node tail = new Node(0, null);
    private Node head;

    // PRED element != null
    // POST a[tail] = element && n' = n + 1 && immutable(head', tail' - 1)
    protected void enqueueImpl(final Object element) {
        Node newElement = tail;
        tail = new Node(element, null);
        if (size() == 0) {
            head = tail;
        } else {
            newElement.next = tail;
        }
    }

    // PRED   size > 0
    // POST   n' = n - 1 && immutable(head', tail') && R = arr[head]
    protected Object dequeueImpl() {
        Object returnElement = head.data;
        head = head.next;
        return returnElement;
    }


    // PRED  size > 0
    // POST  R = elements[head] && immutable(1, elements.length)
    public Object element() {
        return head.data;
    }

    // PRED true
    // POST R = LinkedQueue() size = 0
    public LinkedQueue createQueue(){
        return new LinkedQueue();
    }


}
