package expression.generic;

public abstract class Operation<T> implements AnythingExpression<T> {
    private final AnythingExpression<T> u1;
    private final AnythingExpression<T> u2;
    protected GenCalc<T> op;

    public Operation(AnythingExpression<T> u1, AnythingExpression<T> u2, GenCalc<T> op) {
        this.u1 = u1;
        this.u2 = u2;
        this.op = op;
    }

    public abstract T doOperation(T x, T y);

    public abstract String getSymbol();

    public int hashCode() {
        return (9 * u1.hashCode() + 1) + (5 * u2.hashCode() + 18) + this.getClass().hashCode();
        //return Objects.hash(u1.hashCode()) + Objects.hash(u2.hashCode()) + Objects.hash(getSymbol());
    }

    public boolean equals(Object a) {
        return a != null && this.getClass() == a.getClass()
                && u1.equals(((Operation) a).u1) && u2.equals(((Operation) a).u2);
    }

    public String toString(String oper) {
        return "(" + u1.toString() + " " + oper + " " + u2.toString() + ")";
    }


    public int getPriority() {
        return 1;
    }

    public boolean getLeftBrackets(AnythingExpression<T> u) {
        return false;
    }

    public boolean getRightBrackets(AnythingExpression<T> u) {
        return false;
    }

    public String toMiniString() {
        final StringBuilder str = new StringBuilder();
        if (!(u1 instanceof Operation)) {
            str.append(u1.toMiniString());
        } else {
            if (getLeftBrackets(u1)) {
                str.append(doWithBrackets(u1));
            } else {
                str.append(doWOBrackets(u1));
            }
        }
        str.append(" " + getSymbol() + " ");
        if (!(u2 instanceof Operation)) {
            str.append(u2.toMiniString());
        } else {
            if (getRightBrackets(u2)) {
                str.append(doWithBrackets(u2));
            } else {
                str.append(doWOBrackets(u2));
            }

        }
        return str.toString();
    }

    public String doWithBrackets(AnythingExpression<T> u) {
        return "(" + u.toMiniString() + ")";
    }

    public String doWOBrackets(AnythingExpression<T> u) {
        return u.toMiniString();
    }

    public T evaluate(T value1, T value2, T value3) {
        return doOperation(u1.evaluate(value1, value2, value3), u2.evaluate(value1, value2, value3));
    }

    public T evaluate(T value1) {
        return doOperation(u1.evaluate(value1), u2.evaluate(value1));
    }
}
