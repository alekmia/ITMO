package expression.parser;

import expression.AnythingExpression;

import java.math.BigInteger;

public class TrailingZeros extends UnaryOperation {
    public TrailingZeros(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public Number doOperation(Number x) {
        return Integer.numberOfTrailingZeros(x.intValue());
    }

    @Override
    public String getSymbol() {
        return "t0";
    }

    @Override
    public int getPriority() {
        return 4;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        return false;
    }

    @Override
    public String toString() {
        return super.toString("t0");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("t0");
    }

    @Override
    public int evaluate(int x) {
        return Integer.numberOfTrailingZeros(u1.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Integer.numberOfTrailingZeros(u1.evaluate(x, y, z));
    }

    public BigInteger evaluate(BigInteger x) {
        return BigInteger.valueOf(Integer.numberOfTrailingZeros(u1.evaluate(x).intValue()));
    }
}
