package expression.generic;

public interface AnythingExpression<T> extends Expression<T>, TripleExpression<T> {
    String toMiniString();
}
