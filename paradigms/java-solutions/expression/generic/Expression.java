package expression.generic;

import expression.ToMiniString;

@FunctionalInterface
public interface Expression<T> extends ToMiniString {
    T evaluate(T x);
}
