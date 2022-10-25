package expression.generic;

public class GenUncheckedInteger implements GenCalc<Integer>{

    @Override
    public Integer add(Integer u1, Integer u2) {
        return u1 + u2;
    }

    @Override
    public Integer subtract(Integer u1, Integer u2) {
        return u1 - u2;
    }

    @Override
    public Integer multiply(Integer u1, Integer u2) {
        return u1 * u2;
    }

    @Override
    public Integer divide(Integer u1, Integer u2) {
        return u1 / u2;
    }

    @Override
    public Integer parse(String str) {
        return Integer.parseInt(str);
    }

    @Override
    public Integer negate(Integer u) {
        return -u;
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
