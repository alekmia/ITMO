package expression.exceptions;
import expression.AnythingExpression;
import expression.Divide;
import expression.Operation;
import java.math.BigInteger;

public class CheckedDivide extends Divide {
    public CheckedDivide(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        if (y == 0) {
            throw new OverflowException("Cant Divide By Zero Mate");
        } else if (x == Integer.MIN_VALUE && y == -1) {
            throw new OverflowException("Overflow");
        }
        return x / y;
    }
}