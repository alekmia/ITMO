package expression.exceptions;

import expression.*;
import expression.parser.*;

import java.util.Map;
import java.util.Stack;

public class ExpressionParser implements TripleParser {

    protected static final Map<String, Integer> priorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2,
            "**", 3,
            "//", 3,
            "u-", 4,
            "abs", 4
    );

    protected static final Map<String, Integer> binPriorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2,
            "**", 3,
            "//", 3
    );

    protected static final Map<String, Integer> unaryPriorities = Map.of(
            "u-", 4,
            "abs", 4
    );


    @Override
    public TripleExpression parse(String expression) throws OverflowException {
        Stack<Object> revPolNot = toReversePolishNotation(expression);
        Stack<Object> parseResult = new Stack<>();
        TripleExpression forReturn;

        for (Object currObject : revPolNot) {
            if (binPriorities.containsKey(currObject) || unaryPriorities.containsKey(currObject)) {
                Object x = parseResult.pop();
                if (currObject.equals("u-")) {
                    parseResult.push(new CheckedNegate((AnythingExpression) x));
                    continue;
                } else if (currObject.equals("abs")) {
                    parseResult.push(new CheckedAbs((AnythingExpression) x));
                    continue;
                }
//                else if (currObject.equals("l0")) {
//                    //parseResult.push(new LeadingZeros((AnythingExpression) x));
//                    continue;
//                } else if (currObject.equals("t0")) {
//                    //parseResult.push(new TrailingZeros((AnythingExpression) x));
//                    continue;
//                }
                Object y = parseResult.pop();
                switch ((String) currObject) {
                    case ("+"):
                        parseResult.push(new CheckedAdd((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("-"):
                        parseResult.push(new CheckedSubtract((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("*"):
                        parseResult.push(new CheckedMultiply((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("/"):
                        parseResult.push(new CheckedDivide((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("min"):
                        parseResult.push(new CheckedMinimum((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("max"):
                        parseResult.push(new CheckedMaximum((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("**"):
                        parseResult.push(new CheckedPow((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("//"):
                        parseResult.push(new CheckedLog((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    default:
                }
            } else if (currObject instanceof Integer) {
                parseResult.push(new Const((Integer) currObject));
            } else {
                parseResult.push(new Variable((String) currObject));
            }

        }
        forReturn = (AnythingExpression) parseResult.pop();
        return forReturn;
    }

    public Stack<Object> toReversePolishNotation(String expression) {
        boolean lastWasOper = true;
        boolean wasUnary = false;
        int openedBrackets = 0;
        int startPos = 0;
        int endPos = 0;
        Stack<Object> ans = new Stack<>();
        Stack<String> opers = new Stack<>();
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
                        ans.push(opers.pop());
                    }
                    opers.add(temp);
                    lastWasOper = true;
                    wasUnary = false;
                }
                case 'x', 'y', 'z' -> {
                    ans.add(String.valueOf(expression.charAt(pointer)));
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
                            ans.add(opers.pop());
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
                        ans.add(opers.pop());
                    }
                    if (expression.charAt(pointer) == 'n') {
                        opers.push("min");
                    } else {
                        opers.push("max");
                    }
                    lastWasOper = true;
                    //amInFunc = false;
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
                    int number = 0;
                    while (pointer < expression.length() && Character.isDigit(expression.charAt(pointer))
                            && !priorities.containsKey(String.valueOf(expression.charAt(pointer)))) {
                        int diff = Integer.parseInt(String.valueOf(expression.charAt(pointer)));
                        if (wasUnary) {
                            number = number * 10 - diff;
                        } else {
                            number = number * 10 + diff;
                        }
                        pointer++;
                    }
                    pointer--;
                    if (wasUnary && number != 0) {
                        opers.pop();
                    }
                    if (number > 0 && wasUnary || number < 0 && !wasUnary) {
                        throw new OverflowException("too big of a number to parse at: " + pointer);
                    }
                    ans.add(number);
                    lastWasOper = false;
                    wasUnary = false;
                }

                default -> {
                    throw new OverflowException("no such symbol come on man at: " + pointer);
//                    String temp = "";
//                    while(pointer < expression.length() && (!Character.isWhitespace(expression.charAt(pointer)) && expression.charAt(pointer) != '(')) {
//                        temp += expression.charAt(pointer);
//                        pointer++;
//                    }
//                    if(pointer < expression.length() && expression.charAt(pointer) == '(')
//                        pointer--;
//                    if(binPriorities.containsKey(temp)) {
//                        while (opers.size() != 0 && !opers.peek().equals("(")) {
//                            ans.add(opers.pop());
//                        }
//                        opers.push(temp);
//                        lastWasOper = true;
//                    }
//                    if(unaryPriorities.containsKey(temp)) {
//                        opers.push(temp);
//                        lastWasOper = true;
//                    }
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
                ans.push(pushable);
        }
        return ans;
    }
}
