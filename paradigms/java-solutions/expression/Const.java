package expression;

import java.math.BigInteger;

public class Const implements AnythingExpression {
    private BigInteger constant;

    public Const(int constant) {
        this.constant = BigInteger.valueOf(constant);
    }

    public Const(BigInteger constant) {
        this.constant = constant;
    }

    public String toString() {
        return constant.toString();
    }

    public String toMiniString() {
        return constant.toString();
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(constant.intValue());
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
    public int evaluate(int value1, int value2, int value3) {
        return constant.intValue();
    }

    public int evaluate(int value) {
        return constant.intValue();
    }
}
