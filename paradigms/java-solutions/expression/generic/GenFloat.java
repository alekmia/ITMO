package expression.generic;

public class GenFloat implements GenCalc<Float>{

    @Override
    public Float add(Float u1, Float u2) {
        return u1 + u2;
    }

    @Override
    public Float subtract(Float u1, Float u2) {
        return u1 - u2;
    }

    @Override
    public Float multiply(Float u1, Float u2) {
        return u1 * u2;
    }

    @Override
    public Float divide(Float u1, Float u2) {
        return u1 / u2;
    }

    @Override
    public Float parse(String str) {
        return Float.parseFloat(str);
    }

    @Override
    public Float negate(Float u) {
        return -u;
    }

    @Override
    public Float getMin(Float x, Float y) {
        return Math.min(x, y);
    }

    @Override
    public Float getMax(Float x, Float y) {
        return Math.max(x, y);
    }

    @Override
    public Float count(Float a) {
        return (float) Integer.bitCount(Float.floatToIntBits(a));
    }
}
