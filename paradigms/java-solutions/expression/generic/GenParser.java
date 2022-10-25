package expression.generic;

import java.math.BigInteger;
import java.util.Map;
import java.util.Stack;

public class GenParser<T> implements TripleParser<T> {
    protected GenCalc<T> op;

    public GenParser(GenCalc<T> op){
        this.op = op;
    }
    protected static final Map<String, Integer> priorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2,
//            "**", 3,
//            "//", 3,
            "u-", 4,
            "abs", 4,
            "count", 4
    );

    protected static final Map<String, Integer> binPriorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2
//            "**", 3,
//            "//", 3
    );

    protected static final Map<String, Integer> unaryPriorities = Map.of(
            "u-", 4,
            "abs", 4,
            "count", 4
    );


    @Override
    public TripleExpression<T> parse(String expression) throws OverflowException {
        Stack<ElemOfStack<T>> revPolNot = toReversePolishNotation(expression);
        Stack<ElemOfStack<T>> parseResult = new Stack<>();
        TripleExpression<T> forReturn;

        for (ElemOfStack<T> currObject : revPolNot) {
            if (currObject.getString() != null
                    && (binPriorities.containsKey(currObject.getString()) || unaryPriorities.containsKey(currObject.getString()))) {
                ElemOfStack<T> x = parseResult.pop();
                if (currObject.getString().equals("u-")) {
                    parseResult.push(new ElemOfStack<T>(new UnaryMinus<T>(x.getEx(), op)));
                    continue;
                }
//                else if (currObject.equals("abs")) {
//                    parseResult.push(new CheckedAbs((AnythingExpression<T>) x));
//                    continue;
//                }
//                else if (currObject.equals("l0")) {
//                    //parseResult.push(new LeadingZeros((AnythingExpression) x));
//                    continue;
//                } else if (currObject.equals("t0")) {
//                    //parseResult.push(new TrailingZeros((AnythingExpression) x));
//                    continue;
//                }
                else if (currObject.getString().equals("count")) {
                    parseResult.push(new ElemOfStack<T>(new Count<T>(x.getEx(), op)));
                    continue;
                }
                ElemOfStack<T> y = parseResult.pop();
                switch ((currObject.getString())) {
                    case ("+"):
                        parseResult.push(new ElemOfStack<T>(new Add<T>(y.getEx(), x.getEx(), op)));
                        break;
                    case ("-"):
                        parseResult.push(new ElemOfStack<T>(new Subtract<T>(y.getEx(), x.getEx(), op)));
                        break;
                    case ("*"):
                        parseResult.push(new ElemOfStack<T>(new Multiply<T>(y.getEx(), x.getEx(), op)));
                        break;
                    case ("/"):
                        parseResult.push(new ElemOfStack<T>(new Divide<T>(y.getEx(), x.getEx(), op)));
                        break;
                    case ("min"):
                        parseResult.push(new ElemOfStack<T>(new Minimum<T>(y.getEx(), x.getEx(), op)));
                        break;
                    case ("max"):
                        parseResult.push(new ElemOfStack<T>(new Maximum<T>(y.getEx(), x.getEx(), op)));
                        break;
//                    case ("**"):
//                        parseResult.push(new CheckedPow((AnythingExpression) y, (AnythingExpression) x));
//                        break;
//                    case ("//"):
//                        parseResult.push(new CheckedLog((AnythingExpression) y, (AnythingExpression) x));
//                        break;
                    default:
                        System.err.println("down here");
                }
            } else if (currObject.getNum() instanceof Integer
                    || currObject.getNum() instanceof Double
                    || currObject.getNum() instanceof BigInteger
                    || currObject.getNum() instanceof Float
                    || currObject.getNum() instanceof Long) {
                parseResult.push(new ElemOfStack<T>(new Const<T>((currObject.getNum()))));
            } else {
                parseResult.push(new ElemOfStack<T>(new Variable<T>((currObject.getString()))));
            }

        }
        forReturn = parseResult.pop().getEx();
        return forReturn;
    }

    public Stack<ElemOfStack<T>> toReversePolishNotation(String expression) {
        boolean lastWasOper = true;
        boolean wasUnary = false;
        int openedBrackets = 0;
        int startPos = 0;
        int endPos = 0;
        Stack<ElemOfStack<T>> ans = new Stack<>();
        Stack<String> opers = new Stack<>();
//        System.err.println("expression: " + expression + " end of expression " + expression);
        for (int pointer = 0; pointer < expression.length(); pointer++) {
            if (Character.isWhitespace(expression.charAt(pointer))) {
                wasUnary = false;
                continue;
            }
            switch (expression.charAt(pointer)) {
                case '+', '*', '/', '-' -> {
                    if (expression.charAt(pointer) == '-') {
                        if (lastWasOper) {
                            opers.push("u-");
                            wasUnary = true;
                            break;
                        }
                    }
                    String temp = String.valueOf(expression.charAt(pointer));
                    if (expression.charAt(pointer) == '*') {
                        if (pointer != expression.length() - 1 && expression.charAt(pointer + 1) == '*') {
                            pointer++;
                            temp = "**";
                        }
                    }
                    if (expression.charAt(pointer) == '/') {
                        if (pointer != expression.length() - 1 && expression.charAt(pointer + 1) == '/') {
                            pointer++;
                            temp = "//";
                        }
                    }
                    if (lastWasOper) {
                        throw new OverflowException("Illegal placement of oper at: " + pointer);
                    }
                    while (opers.size() != 0 && !(opers.peek().equals("("))
                            && priorities.get(opers.peek()) >= priorities.get(temp)) {
                        ans.push(new ElemOfStack<T>(opers.pop()));
                    }
                    opers.add(temp);
                    lastWasOper = true;
                    wasUnary = false;
                }
                case 'x', 'y', 'z' -> {
                    ans.add(new ElemOfStack<T>(String.valueOf(expression.charAt(pointer))));
                    lastWasOper = false;
                    wasUnary = false;
                }
                case ('(') -> {
                    openedBrackets++;
                    opers.push("(");
                    wasUnary = false;
                }
                case (')') -> {
                    if (openedBrackets == 0) {
                        throw new OverflowException("why did you close the BRACKET? at: " + pointer);
                    }
                    openedBrackets--;
                    if (lastWasOper) {
                        throw new OverflowException("Next should be a Const/Var at: " + pointer);
                    }
                    String str = "";
                    while (opers.size() != 0) {
                        if (!((opers.peek()).equals("(")))
                            ans.add(new ElemOfStack<T>(opers.pop()));
                        else
                            break;
                    }
                    if (!(opers.peek()).equals("(")) {
                        throw new OverflowException("no bracket come on dawg at: " + pointer);
                    }
                    opers.pop();
                    wasUnary = false;
                }
                case ('m') -> {
                    if (lastWasOper) {
                        throw new OverflowException("Illegal placement of Min/Max at:" + pointer);
                    }
                    if (pointer != 0 && Character.isDigit(expression.charAt(pointer - 1))) {
                        throw new OverflowException("Illegal Min/Max operation at: " + pointer);
                    }
                    pointer += 2;
                    while (opers.size() != 0 && !opers.peek().equals("(")) {
                        ans.add(new ElemOfStack<T>(opers.pop()));
                    }
                    if (expression.charAt(pointer) == 'n') {
                        opers.push("min");
                    } else {
                        opers.push("max");
                    }
                    lastWasOper = true;
                }
                case ('c') -> {
                    if (pointer != 0 && Character.isDigit(expression.charAt(pointer - 1))) {
                        throw new OverflowException("Illegal count operation at: " + pointer);
                    } else if (pointer >= expression.length() - 4 || (expression.charAt(pointer + 1) != 'o'
                            || expression.charAt(pointer + 2) != 'u'
                            || expression.charAt(pointer + 3) != 'n'
                            || expression.charAt(pointer + 4) != 't')) {
                        throw new OverflowException("Thats not a count oper... at: " + pointer);
                    }
                    opers.push("count");
                    pointer += 4;
                    if (pointer != expression.length() - 3 && (expression.charAt(pointer + 1) != '('
                            && !Character.isWhitespace(expression.charAt(pointer + 1)))) {
                        throw new OverflowException("Illegal count operation at: " + pointer);
                    }
                    lastWasOper = true;
                }
                case ('a') -> {
                    if (pointer != 0 && Character.isDigit(expression.charAt(pointer - 1))) {
                        throw new OverflowException("Illegal Min/Max operation at: " + pointer);
                    } else if (pointer >= expression.length() - 2 || (expression.charAt(pointer + 1) != 'b'
                            || expression.charAt(pointer + 2) != 's')) {
                        throw new OverflowException("Thats not an Abs oper... at: " + pointer);
                    }
                    opers.push("abs");
                    pointer += 2;
                    if (pointer != expression.length() - 1 && (expression.charAt(pointer + 1) != '('
                            && !Character.isWhitespace(expression.charAt(pointer + 1)))) {
                        throw new OverflowException("Illegal Abs operation at: " + pointer);
                    }
                    lastWasOper = true;
                }
//                case 'l', 't' -> {
//                    opers.push(String.valueOf(expression.charAt(pointer)) + "0");
//                    pointer++;
//                    lastWasOper = true;
//                    amInFunc = false;
//                }
                case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
                    if (!lastWasOper) {
                        throw new OverflowException("two numbers/vars in a row? at: " + pointer);
                    }
                    T number;
                    boolean good = false;
                    int first = pointer;
                    while (pointer < expression.length() && (Character.isDigit(expression.charAt(pointer))
                            || expression.charAt(pointer) == '.')
                            && !priorities.containsKey(String.valueOf(expression.charAt(pointer)))) {
                        pointer++;
                        good = true;
                    }
                    if(wasUnary) {
                        number = op.parse("-" + expression.substring(first, pointer));
                    } else {
                        number = op.parse(expression.substring(first, pointer));
                    }
//                        int diff = Integer.parseInt(String.valueOf(expression.charAt(pointer)));
//                        if(integer) {
//                            if (wasUnary) {
//                                number = number * 10 - diff;
//                            } else {
//                                number = number * 10 + diff;
//                            }
//                        } else {
//                          fraction /= 10;
//                          number += (diff) * fraction;
//                        }
//                        if(expression.charAt(pointer) == '.') {
//                            integer = false;
//                        }
//                        pointer++;
//                    }
//                    pointer--;
                    if(good) {
                        pointer--;
                        if(wasUnary) {
                            opers.pop();
                        }
                    }

                    ans.add(new ElemOfStack<T>(number));
                    lastWasOper = false;
                    wasUnary = false;
                }

                default -> {
                    throw new OverflowException("no such symbol come on man at: " + pointer);
                }
            }
        }
        if (wasUnary) {
            throw new OverflowException("why didnt you close the - at the end of the expression");
        }
        if (lastWasOper) {
            throw new OverflowException("why didnt you close the oper? at the end of the expression");
        }
        if (openedBrackets > 0) {
            throw new OverflowException("why didnt you close the BRACKET? at the end of the expression");
        }
        if (openedBrackets < 0) {
            throw new OverflowException("why did you close the BRACKET? at the end of the expression");
        }
        while (opers.size() > 0) {
            String pushable = opers.pop();
            if (pushable.equals("(")) {
                throw new OverflowException("no closing bracket dawg at the end of the expression");
            }
            if (priorities.containsKey(pushable))
                ans.push(new ElemOfStack<T>(pushable));
        }
        return ans;
    }
}
