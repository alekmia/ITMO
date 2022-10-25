package expression.exceptions;
import expression.AnythingExpression;
import expression.Multiply;
import expression.Operation;
import java.math.BigInteger;

public class CheckedMultiply extends Multiply {
    public CheckedMultiply(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        if(x == 0 || y == 0) {
            return 0;
        } else if (x * y / y != x) {
            throw new OverflowException("Overflow");
        } else if (y * x / x != y) {
            throw new OverflowException("Overflow");
        }
        return x * y;
    }
}