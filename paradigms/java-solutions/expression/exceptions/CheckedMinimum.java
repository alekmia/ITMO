package expression.exceptions;
import expression.AnythingExpression;
import expression.parser.Minimum;
import expression.parser.UnaryMinus;

public class CheckedMinimum extends Minimum {
    public CheckedMinimum(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
//        if(x == Integer.MIN_VALUE) {
//            throw new OverflowException("overflow");
//        }
        return Math.min(x, y);
    }
}