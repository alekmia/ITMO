package expression.generic;

public class Const<T> implements AnythingExpression<T> {
    private T constant;

    public Const(T constant) {
        this.constant = constant;
    }

    public String toString() {
        return constant.toString();
    }

    public String toMiniString() {
        return constant.toString();
    }

    @Override
    public boolean equals(Object a) {
        if (a == null) {
            return false;
        }
        if (this.getClass() == a.getClass())
            return constant.equals(((Const) a).constant);
        return false;
    }

    @Override
    public T evaluate(T value1, T value2, T value3) {
        return constant;
    }

    public T evaluate(T value) {
        return constant;
    }
}
