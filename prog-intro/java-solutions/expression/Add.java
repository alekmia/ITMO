package expression;

import java.math.BigInteger;

public class Add extends Operation {
    public Add(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "+";
    }

    @Override
    public int getPriority() {
        return 1;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        return (getPriority() > ((Operation) u).getPriority());
    }

    @Override
    public boolean getLeftBrackets(AnythingExpression u) {
        return (getPriority() > ((Operation) u).getPriority());
    }


    @Override
    public String toString() {
        return super.toString("+");
    }

    public Number doOperation(Number x, Number y) {
        if (x instanceof BigInteger) {
            return ((BigInteger) x).add((BigInteger) y);
        } else {
            return x.intValue() + y.intValue();
        }
    }
//    public int doOperation(int x, int y) {
//        return x + y;
//    }
//
//    public BigInteger doOperation(BigInteger x, BigInteger y) {
//        return x.add(y);
//    }
}
