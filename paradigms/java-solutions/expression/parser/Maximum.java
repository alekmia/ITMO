package expression.parser;

import expression.*;

import java.math.BigInteger;

public class Maximum extends Operation {
    public Maximum(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "max";
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        if(u instanceof Minimum) {
            return true;
        }
        return false;
    }

    @Override
    public boolean getLeftBrackets(AnythingExpression u) {
        return false;
    }


    @Override
    public String toString() {
        return super.toString("max");
    }

    public int doOperation(int x, int y) {
        return Math.max(x, y);
    }
}