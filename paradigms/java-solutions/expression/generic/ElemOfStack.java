package expression.generic;

public class ElemOfStack<T>{
    String string = null;
    AnythingExpression<T> ex = null;
    T num = null;

    public ElemOfStack(String elem) {
        this.string = elem;
    }

    public ElemOfStack(AnythingExpression<T> ex) {
        this.ex = ex;
    }

    public ElemOfStack(T num) {
        this.num = num;
    }

    public String getString() {
        return string;
    }

    public AnythingExpression<T> getEx() {
        return ex;
    }

    public T getNum() {
        return num;
    }
}
