package expression.parser;

import expression.AnythingExpression;

public class Abs extends UnaryOperation {
    public Abs(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public int doOperation(int x) {
        if(x >= 0) {
            return x;
        }
        return -x;
    }

    @Override
    public String toString() {
        return super.toString("abs");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("abs");
    }
//
//    @Override
//    public int evaluate(int x) {
//        return -u1.evaluate(x);
//    }
//
//    @Override
//    public int evaluate(int x, int y, int z) {
//        return -u1.evaluate(x, y, z);
//    }
}
