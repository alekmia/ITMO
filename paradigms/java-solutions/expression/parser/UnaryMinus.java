package expression.parser;

import expression.AnythingExpression;

public class UnaryMinus extends UnaryOperation {
    public UnaryMinus(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public int doOperation(int x) {
        return -x;
    }

    @Override
    public String toString() {
        return super.toString("-");
    }

    @Override
    public String toMiniString() {
        return super.toMiniString("-");
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
