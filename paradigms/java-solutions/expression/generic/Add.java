package expression.generic;

public class Add<T> extends Operation<T> {
    public Add(AnythingExpression<T> u1, AnythingExpression<T> u2, GenCalc<T> op) {
        super(u1, u2, op);
    }

    @Override
    public String getSymbol() {
        return "+";
    }

    @Override
    public int getPriority() {
        return 1;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression<T> u) {
        return (getPriority() > ((Operation<T>) u).getPriority());
    }

    @Override
    public boolean getLeftBrackets(AnythingExpression<T> u) {
        return (getPriority() > ((Operation<T>) u).getPriority());
    }


    @Override
    public String toString() {
        return super.toString("+");
    }

    public T doOperation(T x, T y) {
        return op.add(x, y);
    }
}
