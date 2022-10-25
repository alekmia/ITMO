package expression.generic;

public class GenDouble implements GenCalc<Double>{

    @Override
    public Double add(Double u1, Double u2) {
        return u1 + u2;
    }

    @Override
    public Double subtract(Double u1, Double u2) {
        return u1 - u2;
    }

    @Override
    public Double multiply(Double u1, Double u2) {
        return u1 * u2;
    }

    @Override
    public Double divide(Double u1, Double u2) {
        return u1 / u2;
    }

    @Override
    public Double parse(String str) {
        return Double.parseDouble(str);
    }

    @Override
    public Double negate(Double u) {
        return -u;
    }

    @Override
    public Double getMin(Double x, Double y) {
        return Math.min(x, y);
    }

    @Override
    public Double getMax(Double x, Double y) {
        return Math.max(x, y);
    }

    @Override
    public Double count(Double a) {
        return (double) Long.bitCount(Double.doubleToLongBits(a));
    }
}
