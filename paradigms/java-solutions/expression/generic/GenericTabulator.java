package expression.generic;

public class GenericTabulator implements Tabulator {
    public <T> Object[][][] tab(GenCalc<T> op, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        Object[][][] ret = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        GenParser<T> parser = new GenParser<>(op);
        TripleExpression<T> exp = parser.parse(expression);
        for (int i = 0; i <= x2 - x1; i++) {
            for (int j = 0; j <= y2 - y1; j++) {
                for (int k = 0; k <= z2 - z1; k++) {
                    try {
                        ret[i][j][k] = exp.evaluate(op.parse(Integer.toString(x1 + i)),
                                op.parse(Integer.toString(y1 + j)),
                                op.parse(Integer.toString(z1 + k)));
                    } catch (Exception e) {
                        ret[i][j][k] = null;
                    }
                }
            }
        }
        return ret;
    }

    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        GenCalc<?> op = switch (mode) {
            case "i" -> new GenInteger();
            case "d" -> new GenDouble();
            case "bi" -> new GenBigInteger();
            case "u" -> new GenUncheckedInteger();
            case "l" -> new GenLong();
            case "f" -> new GenFloat();
            default -> throw new IllegalArgumentException();
        };
        return tab(op, expression, x1, x2, y1, y2, z1, z2);
    }
}
