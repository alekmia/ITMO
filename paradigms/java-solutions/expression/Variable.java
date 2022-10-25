package expression;

import java.math.BigInteger;

public class Variable implements AnythingExpression {
    public String var;

    public Variable(String var) {
        this.var = var;
    }

    public String toString() {
        return var;
    }

    public String toMiniString() {
        return var;
    }

    @Override
    public int hashCode() {
        return var.hashCode();
    }

    @Override
    public boolean equals(Object a) {
        if (a == null) {
            return false;
        }
        if (this.getClass() == a.getClass())
            return var.equals(((Variable) a).var);
        return false;
    }

    public int evaluate(int value) {
        return value;
    }

    public int evaluate(int value1, int value2, int value3) {
        switch (var) {
            case ("x"):
                return value1;
            case ("y"):
                return value2;
            case ("z"):
                return value3;
        }
        return -1;
    }


}
