package expression.generic;

public class Minimum<T> extends Operation<T> {
    public Minimum(AnythingExpression<T> u1, AnythingExpression<T> u2, GenCalc<T> op) {
        super(u1, u2, op);
    }

    @Override
    public String getSymbol() {
        return "min";
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression<T> u) {
        if(u instanceof Maximum) {
            return true;
        }
        return false;
    }

    @Override
    public boolean getLeftBrackets(AnythingExpression<T> u) {
        return false;
    }


    @Override
    public String toString() {
        return super.toString("min");
    }

    public T doOperation(T x, T y) {
        return op.getMin(x, y);
    }
}
