package expression.generic;

public class GenInteger implements GenCalc<Integer> {

    public Integer add(Integer x, Integer y) {
        if (x > 0 && y > Integer.MAX_VALUE - x) {
            throw new OverflowException("add 1 Overflow");
        } else if (x < 0 && y < Integer.MIN_VALUE - x) {
            throw new OverflowException("add 2 Overflow");
        }
        return x + y;
    }

    @Override
    public Integer subtract(Integer x, Integer y) {
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

    @Override
    public Integer multiply(Integer x, Integer y) {
        if(x == 0 || y == 0) {
            return 0;
        } else if (x * y / y != x) {
            throw new OverflowException("Overflow");
        } else if (y * x / x != y) {
            throw new OverflowException("Overflow");
        }
        return x * y;
    }

    @Override
    public Integer divide(Integer x, Integer y) {
        if (y == 0) {
            throw new OverflowException("Cant Divide By Zero Mate");
        } else if (x == Integer.MIN_VALUE && y == -1) {
            throw new OverflowException("Overflow");
        }
        return x / y;
    }

    @Override
    public Integer parse(String str) {
        return Integer.parseInt(str);
    }

    @Override
    public Integer negate(Integer x) {
        if(x == Integer.MIN_VALUE) {
            throw new OverflowException("negate overflow");
        }
        return -x;
    }

    @Override
    public Integer getMin(Integer x, Integer y) {
        return Math.min(x, y);
    }

    @Override
    public Integer getMax(Integer x, Integer y) {
        return Math.max(x, y);
    }

    @Override
    public Integer count(Integer a) {
        return Integer.bitCount(a);
    }
}
