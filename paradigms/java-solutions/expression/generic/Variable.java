package expression.generic;


public class Variable<T> implements AnythingExpression<T> {
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
    public boolean equals(Object a) {
        if (a == null) {
            return false;
        }
        if (this.getClass() == a.getClass())
            return var.equals(((Variable) a).var);
        return false;
    }

    public T evaluate(T value) {
        return value;
    }

    public T evaluate(T value1, T value2, T value3) {
        switch (var) {
            case ("x"):
                return value1;
            case ("y"):
                return value2;
            case ("z"):
                return value3;
        }
        return null;
    }


}
