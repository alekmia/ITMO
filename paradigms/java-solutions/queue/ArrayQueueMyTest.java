package queue;

public class ArrayQueueMyTest {
    public static void main(String[] args) {
        ArrayQueue stackTest = new ArrayQueue();
        for (int i = 0; i < 5; i++) {
            stackTest.enqueue("e" + i);
        }
        stackTest.enqueue("e" + 10);
        while (!stackTest.isEmpty()) {
            System.out.println(stackTest.size() + " " + stackTest.dequeue());
        }

        stackTest.clear();
        System.out.println(stackTest.size());
    }
}
