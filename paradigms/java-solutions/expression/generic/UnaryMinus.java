package expression.generic;

public class UnaryMinus<T> extends UnaryOperation<T> {
    public UnaryMinus(AnythingExpression<T> u1, GenCalc<T> op) {
        super(u1, op);
    }

    @Override
    public T doOperation(T x) {
        return op.negate(x);
    }

    @Override
    public String toString() {
        return super.toString("-");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("-");
    }

}
