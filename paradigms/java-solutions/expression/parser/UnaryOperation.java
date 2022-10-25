package expression.parser;

import expression.AnythingExpression;
import expression.Operation;

import java.math.BigInteger;

public abstract class UnaryOperation implements AnythingExpression {
    protected AnythingExpression u1;

    public UnaryOperation(AnythingExpression u1) {
        this.u1 = u1;
    }

    public abstract int doOperation(int x);

    public int hashCode() {
        return 9 * u1.hashCode() + this.getClass().hashCode();
    }

    public boolean equals(Object a) {
        return a != null && this.getClass() == a.getClass()
                && u1.equals(((expression.parser.UnaryOperation) a).u1);
    }

    public String toString(String oper) {
        return oper + "(" + u1.toString() + ")";
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

    public int evaluate(int value1, int value2, int value3) {
        return (int) doOperation(u1.evaluate(value1, value2, value3));
    }

    public int evaluate(int value1) {
        return (int) doOperation(u1.evaluate(value1));
    }


}

