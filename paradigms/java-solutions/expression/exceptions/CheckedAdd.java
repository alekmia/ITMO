package expression.exceptions;
import expression.Add;
import expression.AnythingExpression;
import expression.Operation;
import java.math.BigInteger;

public class CheckedAdd extends Add {
    public CheckedAdd(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        if (x > 0 && y > Integer.MAX_VALUE - x) {
            throw new OverflowException("Overflow");
        } else if (x < 0 && y < Integer.MIN_VALUE - x) {
            throw new OverflowException("Overflow");
        }
        return x + y;
    }
}