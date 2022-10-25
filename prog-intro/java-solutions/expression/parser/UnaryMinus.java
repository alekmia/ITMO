package expression.parser;

import expression.AnythingExpression;
import expression.Const;

import java.math.BigInteger;

public class UnaryMinus extends UnaryOperation {
    public UnaryMinus(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public Number doOperation(Number x) {
        return 0 - x.longValue();
    }

    @Override
    public String getSymbol() {
        return "";
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        return false;
    }

    @Override
    public String toString() {
        return super.toString("-");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("-");
    }

    @Override
    public int evaluate(int x) {
        return 0 - u1.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return 0 - u1.evaluate(x, y, z);
    }

    public BigInteger evaluate(BigInteger x) {
        return (BigInteger.ZERO).subtract(u1.evaluate(x));
    }


}
