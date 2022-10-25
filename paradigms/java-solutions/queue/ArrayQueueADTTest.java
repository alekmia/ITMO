package queue;

public class ArrayQueueADTTest {
    public static void main(String[] args) {
        ArrayQueueADT queueTest = new ArrayQueueADT();
        for (int i = 0; i < 5; i++) {
            ArrayQueueADT.enqueue(queueTest, "e" + i);
        }
        ArrayQueueADT.enqueue(queueTest, "e" + 10);
        while (!ArrayQueueADT.isEmpty(queueTest)) {
            System.out.println(ArrayQueueADT.size(queueTest) + " " + ArrayQueueADT.dequeue(queueTest));
        }
        ArrayQueueADT.clear(queueTest);
        System.out.println(ArrayQueueADT.size(queueTest));
    }
}
