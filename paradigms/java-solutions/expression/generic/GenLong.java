package expression.generic;

public class GenLong implements GenCalc<Long>{

    @Override
    public Long add(Long u1, Long u2) {
        return u1 + u2;
    }

    @Override
    public Long subtract(Long u1, Long u2) {
        return u1 - u2;
    }

    @Override
    public Long multiply(Long u1, Long u2) {
        return u1 * u2;
    }

    @Override
    public Long divide(Long u1, Long u2) {
        return u1 / u2;
    }

    @Override
    public Long parse(String str) {
        return Long.parseLong(str);
    }

    @Override
    public Long negate(Long u) {
        return -u;
    }

    @Override
    public Long getMin(Long x, Long y) {
        return Math.min(x, y);
    }

    @Override
    public Long getMax(Long x, Long y) {
        return Math.max(x, y);
    }

    @Override
    public Long count(Long a) {
        return (long) Long.bitCount(a);
    }
}
