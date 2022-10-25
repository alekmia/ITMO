package expression.exceptions;
import expression.AnythingExpression;
import expression.parser.UnaryMinus;

public class CheckedNegate extends UnaryMinus {
    public CheckedNegate(AnythingExpression u1) {
        super(u1);
    }

    @Override
    public int doOperation(int x) {
        if(x == Integer.MIN_VALUE) {
            throw new OverflowException("overflow");
        }
        return -x;
    }
}