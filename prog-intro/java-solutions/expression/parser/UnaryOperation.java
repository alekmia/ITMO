package expression.parser;

import expression.AnythingExpression;
import expression.Operation;

import java.math.BigInteger;

public abstract class UnaryOperation implements AnythingExpression {
    protected AnythingExpression u1;

    public UnaryOperation(AnythingExpression u1) {
        this.u1 = u1;
    }

    public abstract Number doOperation(Number x);
//    public abstract int doOperation(int x, int y);
//    public abstract BigInteger doOperation(BigInteger x, BigInteger y);
    public abstract String getSymbol();

    public int hashCode() {
        return (9 * u1.hashCode() + 1) + this.getClass().hashCode();
        //return Objects.hash(u1.hashCode()) + Objects.hash(u2.hashCode()) + Objects.hash(getSymbol());
    }

    public boolean equals(Object a) {
        return a != null && this.getClass() == a.getClass()
                && u1.equals(((expression.parser.UnaryOperation) a).u1);
    }

    public String toString(String oper) {
        return oper + "(" + u1.toString() + ")";
    }


    public int getPriority() {
        return 0;
    }

    public boolean getRightBrackets(AnythingExpression u) {
        return false;
    }

    public String toMiniString(String oper) {
        StringBuilder str = new StringBuilder();
        str.append(oper);
        if (!(u1 instanceof expression.Operation)) {
            str.append(" " + u1.toMiniString());
        } else {
            str.append(doWithBrackets(u1));
        }
        return str.toString();
    }

    public String doWithBrackets(AnythingExpression u) {
        return "(" + u.toMiniString() + ")";
    }

//    public int evaluate(int value1) {
//        return evaluateAll((Number)value1).intValue();
//    }
//
//    public BigInteger evaluate(BigInteger value1) {
//        return (BigInteger)evaluateAll((Number)value1);
//    }
//
//    public Number evaluateAll(Number value1) {
//        if(value1 instanceof BigInteger) {
//            return doOperation(u1.evaluate((BigInteger)value1), u2.evaluate((BigInteger)value1));
//        } else {
//            return doOperation(u1.evaluate((int)value1), u2.evaluate((int)value1));
//        }
//    }

    public int evaluate(int value1, int value2, int value3) {
        return (int) doOperation(u1.evaluate(value1, value2, value3));
    }

    public int evaluate(int value1) {
        return (int) doOperation(u1.evaluate(value1));
    }

    public BigInteger evaluate(BigInteger value1) {
        return (BigInteger) doOperation(u1.evaluate(value1));
    }

}

