package expression;

import java.math.BigInteger;

public class Multiply extends Operation {
    public Multiply(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "*";
    }

    public int getPriority() {
        return 2;
    }

    public boolean getRightBrackets(AnythingExpression u) {
        return !getSymbol().equals(((Operation)u).getSymbol()) && getPriority() >= ((Operation) u).getPriority();
    }

    public boolean getLeftBrackets(AnythingExpression u) {
        return getPriority() > ((Operation) u).getPriority() && ((Operation) u).getPriority() != 2;
    }

    public String toString() {
        return super.toString("*");
    }

    public int doOperation(int x, int y) {
        return x * y;
    }
//
//    public BigInteger doOperation(BigInteger x, BigInteger y) {
//        return x.multiply(y);
//    }
}
