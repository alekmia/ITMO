package expression.generic;


public abstract class UnaryOperation<T> implements AnythingExpression<T> {
    protected AnythingExpression<T> u1;
    protected GenCalc<T> op;

    public UnaryOperation(AnythingExpression<T> u1, GenCalc<T> op) {
        this.u1 = u1;
        this.op = op;
    }

    public abstract T doOperation(T x);

    public int hashCode() {
        return 9 * u1.hashCode() + this.getClass().hashCode();
    }

    public boolean equals(Object a) {
        return a != null && this.getClass() == a.getClass()
                && u1.equals(((UnaryOperation<T>) a).u1);
    }

    public String toString(String oper) {
        return oper + "(" + u1.toString() + ")";
    }

    public String toMiniString(String oper) {
        StringBuilder str = new StringBuilder();
        str.append(oper);
        if (!(u1 instanceof Operation)) {
            str.append(" " + u1.toMiniString());
        } else {
            str.append(doWithBrackets(u1));
        }
        return str.toString();
    }

    public String doWithBrackets(AnythingExpression<T> u) {
        return "(" + u.toMiniString() + ")";
    }

    public T evaluate(T value1, T value2, T value3) {
        return doOperation(u1.evaluate(value1, value2, value3));
    }

    public T evaluate(T value1) {
        return doOperation(u1.evaluate(value1));
    }


}

