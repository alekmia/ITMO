package expression.generic;


public class Multiply<T> extends Operation<T> {
    public Multiply(AnythingExpression<T> u1, AnythingExpression<T> u2, GenCalc<T> op) {
        super(u1, u2, op);
    }

    @Override
    public String getSymbol() {
        return "*";
    }

    public int getPriority() {
        return 2;
    }

    public boolean getRightBrackets(AnythingExpression<T> u) {
        return !getSymbol().equals(((Operation<T>)u).getSymbol()) && getPriority() >= ((Operation<T>) u).getPriority();
    }

    public boolean getLeftBrackets(AnythingExpression<T> u) {
        return getPriority() > ((Operation<T>) u).getPriority() && ((Operation<T>) u).getPriority() != 2;
    }

    public String toString() {
        return super.toString("*");
    }

    public T doOperation(T x, T y) {
        return op.multiply(x, y);
    }
}
