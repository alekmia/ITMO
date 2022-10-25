package expression.generic;

public class Maximum<T> extends Operation<T> {
    public Maximum(AnythingExpression<T> u1, AnythingExpression<T> u2, GenCalc<T> op) {
        super(u1, u2, op);
    }

    @Override
    public String getSymbol() {
        return "max";
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression<T> u) {
        if(u instanceof Minimum) {
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
        return super.toString("max");
    }

    public T doOperation(T x, T y) {
        return op.getMax(x, y);
    }
}