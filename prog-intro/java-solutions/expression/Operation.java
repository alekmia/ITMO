package expression;

import expression.common.Op;

import java.math.BigInteger;
import java.util.Map;
import java.util.Objects;

public abstract class Operation implements AnythingExpression {
    private final AnythingExpression u1;
    private final AnythingExpression u2;

    public Operation(AnythingExpression u1, AnythingExpression u2) {
        this.u1 = u1;
        this.u2 = u2;
    }

    public abstract Number doOperation(Number x, Number y);

    public abstract String getSymbol();

    public int hashCode() {
        return (9 * u1.hashCode() + 1) + (5 * u2.hashCode() + 18) + this.getClass().hashCode();
        //return Objects.hash(u1.hashCode()) + Objects.hash(u2.hashCode()) + Objects.hash(getSymbol());
    }

    public boolean equals(Object a) {
        return a != null && this.getClass() == a.getClass()
                && u1.equals(((Operation) a).u1) && u2.equals(((Operation) a).u2);
    }

    public String toString(String oper) {
        return "(" + u1.toString() + " " + oper + " " + u2.toString() + ")";
    }


    public int getPriority() {
        return 1;
    }

    public boolean getLeftBrackets(AnythingExpression u) {
        return false;
    }

    public boolean getRightBrackets(AnythingExpression u) {
        return false;
    }

    public String toMiniString() {
        final StringBuilder str = new StringBuilder();

        if (!(u1 instanceof Operation)) {
            str.append(u1.toMiniString());
        } else {
            if (getLeftBrackets(u1)) {
                str.append(doWithBrackets(u1));
            } else {
                str.append(doWOBrackets(u1));
            }

        }

        str.append(" " + getSymbol() + " ");

        if (!(u2 instanceof Operation)) {

            str.append(u2.toMiniString());
        } else {
            if (getRightBrackets(u2)) {
                str.append(doWithBrackets(u2));
            } else {
                str.append(doWOBrackets(u2));
            }

        }
        return str.toString();
    }

    public String doWithBrackets(AnythingExpression u) {
        return "(" + u.toMiniString() + ")";
    }

    public String doWOBrackets(AnythingExpression u) {
        return u.toMiniString();
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
        return (int) doOperation(u1.evaluate(value1, value2, value3), u2.evaluate(value1, value2, value3));
    }

    public int evaluate(int value1) {
        return (int) doOperation(u1.evaluate(value1), u2.evaluate(value1));
    }

    public BigInteger evaluate(BigInteger value1) {
        return (BigInteger) doOperation(u1.evaluate(value1), u2.evaluate(value1));
    }

}
