package expression.exceptions;
import expression.AnythingExpression;
import expression.parser.Log;

public class CheckedLog extends Log {
    public CheckedLog(AnythingExpression u1, AnythingExpression u2) {
        super(u1, u2);
    }

    public int doOperation(int x, int y) {
        if(x <= 0 || y <= 0 || y == 1 ) {
            throw new OverflowException("Wrong log initiation");
        }
        return (int)(Math.log(x) / Math.log(y));
    }
}