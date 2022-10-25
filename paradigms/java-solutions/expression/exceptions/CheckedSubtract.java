package expression.exceptions;
import expression.AnythingExpression;
import expression.Subtract;
import expression.Operation;
import java.math.BigInteger;

public class CheckedSubtract extends Subtract {
    public CheckedSubtract(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        //System.err.println(x + " - " + y);
        if (x > 0 && y < 0 && x - y < 0) {
            throw new OverflowException("overflow");
        } else if (x < 0 && y > 0 && x - y > 0){
            throw new OverflowException("overflow");
        }
        if (x == 0 && y == Integer.MIN_VALUE) {
            throw new OverflowException("overflow");
        }
        return x - y;
    }
}