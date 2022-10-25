package expression;

import java.math.BigInteger;

public class Divide extends Operation {

    public Divide(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "/";
    }

    public int getPriority() {
        return 2;
    }

    public boolean getRightBrackets(AnythingExpression u) {
        return true;
    }

    public boolean getLeftBrackets(AnythingExpression u) {
        return getPriority() > ((Operation) u).getPriority();
    }


    public String toString() {
        return super.toString("/");
    }

    public Number doOperation(Number x, Number y) {
        if (x instanceof BigInteger) {
            return ((BigInteger) x).divide((BigInteger) y);
        } else {
            return x.intValue() / y.intValue();
        }
    }

//    public int doOperation(int x, int y) {
//        return x / y;
//    }
//
//    public BigInteger doOperation(BigInteger x, BigInteger y) {
//        return x.divide(y);
//    }
}
