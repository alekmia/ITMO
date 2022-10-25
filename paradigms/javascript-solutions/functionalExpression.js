"use strict"
const operation = doOper => (...value) => (...args) => doOper(...value.map(arg => arg(...args)))

const varNames = {"x": 0, "y": 1, "z": 2}

const cnst = (value) => () => value;
const variable = (value) => (...args) => args[varNames[value]];
const negate = operation((value) => -value);
const abs = operation((value) => Math.abs(value));

const add = operation((arg1, arg2) => arg1 + arg2);
const subtract = operation((arg1, arg2) => arg1 - arg2);
const multiply = operation((arg1, arg2) => arg1 * arg2);
const divide = operation((arg1, arg2) => arg1 / arg2);
const pi = () => Math.PI;
const e = () => Math.E;

const iff = operation((value1, value2, value3) => value1 >= 0 ? value2 : value3);

const oper = {"+": [add, 2], "-": [subtract, 2], "*": [multiply, 2], "/": [divide, 2], "negate": [negate, 1], "abs": [abs, 1], "iff": [iff, 3]}

const constList = {"pi": Math.PI, "e": Math.E}
const parse = parsable => {
    return parsable.split(" ").filter(token => token !== "").reduce(parse2, []).pop();
}

const parse2 = (parseStack, token) => {
    let intToken = parseInt(token);
    if (token in constList) {
        parseStack.push(cnst(constList[token]))
    } else if (token in varNames) {
        parseStack.push(variable(token));
    } else if (token in oper) {
        let num = oper[token][1];
        let arr = parseStack.slice(parseStack.length - num);
        parseStack.splice(parseStack.length - num, arr.length, oper[token][0](...arr));
    } else {
        parseStack.push(cnst(intToken));
    }
    return parseStack;
}



