package expression.parser;

import expression.AnythingExpression;
import expression.TripleExpression;

import java.math.BigInteger;
import java.util.*;

import expression.*;

public class ExpressionParser implements Parser {

    protected static final Map<String, Integer> priorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2,
            "u-", 4,
            "l0", 4,
            "t0", 4
    );

    protected static final Map<String, Integer> binPriorities = Map.of(
            "min", 0,
            "max", 0,
            "+", 1,
            "-", 1,
            "*", 2,
            "/", 2
    );

    protected static final Map<String, Integer> unaryPriorities = Map.of(
            "u-", 4,
            "l0", 4,
            "t0", 4
    );


    @Override
    public TripleExpression parse(String expression) {
        Stack<Object> revPolNot = toReversePolishNotation(expression);
        Stack<Object> parseResult = new Stack<>();
        TripleExpression forReturn;


        for (Object currObject : revPolNot) {
            if (binPriorities.containsKey(currObject) || unaryPriorities.containsKey(currObject)) {
                Object x = parseResult.pop();
                if (currObject.equals("u-")) {
                    parseResult.push(new UnaryMinus((AnythingExpression) x));
                    continue;
                }
//                else if (currObject.equals("l0")) {
//                    parseResult.push(new LeadingZeros((AnythingExpression) x));
//                    continue;
//                } else if (currObject.equals("t0")) {
//                    parseResult.push(new TrailingZeros((AnythingExpression) x));
//                    continue;
//                }
                Object y = parseResult.pop();
                switch ((String) currObject) {
                    case ("+"):
                        parseResult.push(new Add((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("-"):
                        parseResult.push(new Subtract((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("*"):
                        parseResult.push(new Multiply((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("/"):
                        parseResult.push(new Divide((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("min"):
                        parseResult.push(new Minimum((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    case ("max"):
                        parseResult.push(new Maximum((AnythingExpression) y, (AnythingExpression) x));
                        break;
                    default:
                        System.err.println("down here");
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
        int startPos = 0;
        int endPos = 0;
        Stack<Object> ans = new Stack<>();
        Stack<String> opers = new Stack<>();
        System.err.println("expression: " + expression);
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
                    while (opers.size() != 0 && !(opers.peek().equals("("))
                            && priorities.get(opers.peek()) >= priorities.get(String.valueOf(expression.charAt(pointer)))) {
                        ans.push(opers.pop());
                    }
                    opers.add(String.valueOf(expression.charAt(pointer)));
                    lastWasOper = true;
                    wasUnary = false;
                }
                case 'x', 'y', 'z' -> {
                    ans.add(String.valueOf(expression.charAt(pointer)));
                    lastWasOper = false;
                    wasUnary = false;
                }
                case ('(') -> {
                    opers.push("(");
                    wasUnary = false;
                }
                case (')') -> {
                    String str = "";
                    while (opers.size() != 0) {
                        if (!((opers.peek()).equals("(")))
                            ans.add(opers.pop());
                        else
                            break;
                    }
                    opers.pop();
                    wasUnary = false;
                }
//                case ('m') -> {
//                    pointer += 2;
//                    while (opers.size() != 0 && !opers.peek().equals("(")) {
//                        ans.add(opers.pop());
//                    }
//                    if (expression.charAt(pointer) == 'n') {
//                        opers.push("min");
//                    } else {
//                        opers.push("max");
//                    }
//                    lastWasOper = true;
//                    amInFunc = false;
//                }
//                case 'l', 't' -> {
//                    opers.push(String.valueOf(expression.charAt(pointer)) + "0");
//                    pointer++;
//                    lastWasOper = true;
//                    amInFunc = false;
//                }
                case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
                    int number = 0;
                    while (pointer < expression.length() && Character.isDigit(expression.charAt(pointer))
                            && !priorities.containsKey(String.valueOf(expression.charAt(pointer)))) {
                        int diff = Integer.parseInt(String.valueOf(expression.charAt(pointer)));
                        if (wasUnary) {
                            number = number * 10 - diff;
                        }  else {
                            number = number * 10 + diff;
                        }
                        pointer++;
                    }
                    pointer--;
                    if (wasUnary && number != 0) {
                        opers.pop();
                    }
                    ans.add(number);
                    lastWasOper = false;
                    wasUnary = false;
                }
                default -> {
                    String temp = "";
                    while(pointer < expression.length() && (!Character.isWhitespace(expression.charAt(pointer)) && expression.charAt(pointer) != '(')) {
                        temp += expression.charAt(pointer);
                        System.err.println("[: " + expression.charAt(pointer));
                        pointer++;
                    }
                    if(pointer < expression.length() && expression.charAt(pointer) == '(')
                        pointer--;
                    if(binPriorities.containsKey(temp)) {
                        while (opers.size() != 0 && !opers.peek().equals("(")) {
                            ans.add(opers.pop());
                        }
                        opers.push(temp);
                        lastWasOper = true;
                    }
                    if(unaryPriorities.containsKey(temp)) {
                        opers.push(temp);
                        lastWasOper = true;
                    }
                }
            }
            for(Object arsen : opers) {
                System.err.println(arsen);
            }
        }
        while (opers.size() > 0) {
            String pushable = opers.pop();
            if (priorities.containsKey(pushable))
                ans.push(pushable);
        }
        return ans;
    }
}
