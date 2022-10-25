package expression.exceptions;
import expression.AnythingExpression;
import expression.parser.Pow;

public class CheckedPow extends Pow {
    public CheckedPow(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        if((x == 0 && y <= 0) || (x != 0 && y < 0)) {
            throw new OverflowException("0 as pow");
        }
        if(x > 0 && y > Math.log(Integer.MAX_VALUE) / Math.log(x)) {
            throw new OverflowException("overflow");
        } else if(x == Integer.MIN_VALUE && (y != 1 && y != 0)) {
            throw new OverflowException("overflow");
        } else if(x != Integer.MIN_VALUE && x < 0 && y > Math.log(Integer.MAX_VALUE) / Math.log(Math.abs(x))) {
            throw new OverflowException("overflow");
        }
        return (int)Math.pow((double)x, (double)y);
    }
}