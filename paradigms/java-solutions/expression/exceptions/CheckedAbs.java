package expression.exceptions;
import expression.AnythingExpression;
import expression.parser.Abs;

public class CheckedAbs extends Abs {
    public CheckedAbs(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public int doOperation(int x) {
        if (x == Integer.MIN_VALUE) {
            throw new OverflowException("overflow");
        } else if (x >= 0) {
            return x;
        }
        return -x;
    }
}