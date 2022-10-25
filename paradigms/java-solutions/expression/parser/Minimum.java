package expression.parser;

import expression.*;

import java.math.BigInteger;

public class Minimum extends Operation {
    public Minimum(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "min";
    }

    @Override
    public int getPriority() {
        return 0;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        if(u instanceof Maximum) {
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
        return super.toString("min");
    }

    public int doOperation(int x, int y) {
        return Math.min(x, y);
    }
}
