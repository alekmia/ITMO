package expression.parser;

import expression.*;

public class Pow extends Operation {
    public Pow(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    @Override
    public String getSymbol() {
        return "**";
    }

    @Override
    public int getPriority() {
        return 5;
    }

    @Override
    public boolean getRightBrackets(AnythingExpression u) {
        return getPriority() >= ((Operation) u).getPriority();
    }

    @Override
    public boolean getLeftBrackets(AnythingExpression u) {
        return getPriority() > ((Operation) u).getPriority();
    }


    @Override
    public String toString() {
        return super.toString("**");
    }

    public int doOperation(int x, int y) {
        return (int)Math.pow((double)x, (double)y);
    }
}
