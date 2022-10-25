package expression.parser;

import expression.AnythingExpression;

import java.math.BigInteger;

public class LeadingZeros extends UnaryOperation {
    public LeadingZeros(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public Number doOperation(Number x) {
        return Integer.numberOfLeadingZeros(x.intValue());
    }

    @Override
    public String getSymbol() {
        return "l0";
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
        return super.toString("l0");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("l0");
    }

    @Override
    public int evaluate(int x) {
        return Integer.numberOfLeadingZeros(u1.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Integer.numberOfLeadingZeros(u1.evaluate(x, y, z));
    }

    public BigInteger evaluate(BigInteger x) {
        return BigInteger.valueOf(Integer.numberOfLeadingZeros(u1.evaluate(x).intValue()));
    }
}
