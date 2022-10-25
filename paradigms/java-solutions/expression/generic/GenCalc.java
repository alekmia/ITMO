package expression.generic;

public interface GenCalc<T> {
    T add(T u1, T u2);
    T subtract(T u1, T u2);
    T multiply(T u1, T u2);
    T divide(T u1, T u2);
    T parse(String str);
    T negate(T u);
    T getMin(T u1, T u2);
    T getMax(T u1, T u2);
    T count(T u1);
}