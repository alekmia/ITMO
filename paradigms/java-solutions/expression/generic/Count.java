package expression.generic;

public class Count<T> extends UnaryOperation<T> {
    public Count(AnythingExpression<T> u1, GenCalc<T> op) {
        super(u1, op);
    }

    @Override
    public T doOperation(T x) {
        return op.count(x);
    }

    @Override
    public String toString() {
        return super.toString("count");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("count");
    }

}
