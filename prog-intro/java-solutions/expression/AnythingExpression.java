package expression;


public interface AnythingExpression extends BigIntegerExpression, Expression, TripleExpression {
    String toMiniString();
}
