package expression.generic;

import java.math.BigInteger;

public class GenBigInteger implements GenCalc<BigInteger> {
    @Override
    public BigInteger add(BigInteger u1, BigInteger u2) {
        return u1.add(u2);
    }

    @Override
    public BigInteger subtract(BigInteger u1, BigInteger u2) {
        return u1.subtract(u2);
    }

    @Override
    public BigInteger multiply(BigInteger u1, BigInteger u2) {
        return u1.multiply(u2);
    }

    @Override
    public BigInteger divide(BigInteger u1, BigInteger u2) {
        return u1.divide(u2);
    }

    @Override
    public BigInteger parse(String str) {
        return new BigInteger(str);
    }

    @Override
    public BigInteger negate(BigInteger u) {
        return u.negate();
    }

    @Override
    public BigInteger getMin(BigInteger x, BigInteger y) {
        if(x.compareTo(y) > 0) {
            return y;
        }
        return x;
    }

    @Override
    public BigInteger getMax(BigInteger x, BigInteger y) {
        if(x.compareTo(y) < 0) {
            return y;
        }
        return x;
    }

    @Override
    public BigInteger count(BigInteger a) {
        return new BigInteger(Integer.toString(a.bitCount()));
    }
}
