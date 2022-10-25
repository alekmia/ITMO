package expression.generic;

@FunctionalInterface
public interface TripleParser<T> {
    TripleExpression<T> parse(String expression) throws Exception;
}
