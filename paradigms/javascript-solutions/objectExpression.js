"use strict";

function ExpressionPro(doOper, operator, calculateDiff, ...values) {
    this.doOper = doOper;
    this.values = values;
    this.operator = operator;
    this.calculateDiff = calculateDiff;
}

ExpressionPro.prototype.evaluate = function (...number) {
    return this.doOper(...this.values.map(arg => arg.evaluate(...number)))
}
ExpressionPro.prototype.toString = function () {
    const sumWithInitial = this.values.reduce(
        (previousValue, currentValue) => previousValue.toString() + " " + currentValue.toString()
    );
    return sumWithInitial + " " + this.operator;
}
ExpressionPro.prototype.prefix = function () {
    let sumWithInitial = this.values.map(value => value.prefix()).reduce(
        (previousValue, currentValue) => previousValue + " " + currentValue
    );
    return "(" + this.operator + " " + sumWithInitial + ")";
}
ExpressionPro.prototype.postfix = function () {
    let sumWithInitial = this.values.map(value => value.postfix()).reduce(
        (previousValue, currentValue) => previousValue + " " + currentValue
    );
    return "(" + sumWithInitial + " " + this.operator + ")";
}
ExpressionPro.prototype.diff = function (parameter) {
    return this.calculateDiff(parameter, ...this.values);
}


function classFabric(doOper, operator, calculateDiff) {
    function ExpressionTmp(...values) {
        ExpressionPro.call(this, doOper, operator, calculateDiff, ...values);
    }

    ExpressionTmp.prototype = Object.create(ExpressionPro.prototype);
    return ExpressionTmp;
}

const Add = classFabric((value1, value2) => value1 + value2, "+", diffAdd);

function diffAdd(parameter, left, right) {
    return new Add(left.diff(parameter), right.diff(parameter));
}

const Subtract = classFabric((value1, value2) => value1 - value2, "-", diffSubtract)

function diffSubtract(parameter, left, right) {
    return new Subtract(left.diff(parameter), right.diff(parameter));
}

const Multiply = classFabric((value1, value2) => value1 * value2, "*", diffMultiply)

function diffMultiply(parameter, left, right) {
    return new Add(new Multiply(left.diff(parameter), right), new Multiply(left, right.diff(parameter)));
}

const Divide = classFabric((value1, value2) => value1 / value2, "/", diffDivide);

function diffDivide(parameter, left, right) {
    return new Divide(new Subtract(new Multiply(left.diff(parameter), right), new Multiply(left, right.diff(parameter))), new Multiply(right, right));
}

const Negate = classFabric((value) => -value, "negate", diffNegate)

function diffNegate(parameter, value) {
    return new Negate(value.diff(parameter));
}

const Mean = classFabric((...values) => values.reduce((arg1, arg2) => arg1 + arg2) / values.length, "mean", diffMean)

function diffMean(parameter, ...values) {
    let sum = values[0].diff(parameter)
    for (let i = 1; i < values.length; i++) {
        sum = new Add(sum, values[i].diff(parameter));
    }
    return new Divide(sum, new Const(values.length));
}

const Var = classFabric(
    function (...values) {
        let mean = values.reduce((arg1, arg2) => arg1 + arg2) / values.length;
        return values.map(arg => arg - mean).map(arg => arg * arg).reduce((arg1, arg2) => arg1 + arg2) / values.length;
    },
    "var", diffVar);

function diffVar(parameter, ...values) {
    return new Subtract(
        new Mean(...values.map(arg => new Multiply(arg, arg))),
        new Pow(new Mean(...values), new Const(2))
    ).diff(parameter);
}


function Const(value) {
    this.value = value;
}

Const.prototype.evaluate = function () {
    return this.value;
}
Const.prototype.toString = function () {
    return this.value.toString();
};
Const.prototype.prefix = function () {
    return this.value.toString();
}
Const.prototype.postfix = function () {
    return this.value.toString();
}
Const.prototype.diff = function () {
    return new Const(0);
};


function Variable(value) {
    this.value = value;
}

Variable.prototype.evaluate = function (...number) {
    return number[varNames[this.value]];
}
Variable.prototype.toString = function () {
    return this.value;
};
Variable.prototype.prefix = function () {
    return this.value.toString();
}
Variable.prototype.postfix = function () {
    return this.value.toString();
}
Variable.prototype.diff = function (parameter) {
    if (parameter === this.value) {
        return new Const(1);
    }
    return new Const(0);
};


let E = new Const(Math.E);


const Log = classFabric((left, right) => right * left === 0 ? 0 : Math.log(Math.abs(right)) / Math.log(Math.abs(left)), "log", diffLog)

function diffLog(parameter, left, right) {
    if (left === E) {
        return new Divide(right.diff(parameter), right)
    }
    return new Divide(new Log(E, right), new Log(E, left)).diff(parameter);
}


const Pow = classFabric((left, right) => Math.pow(left, right), "pow", diffPow)

function diffPow(parameter, left, right) {
    return new Multiply(new Pow(left, right), new Multiply(new Log(E, left), right).diff(parameter))
}

const varNames = {'x': 0, 'y': 1, 'z': 2};

const oper = {
    "var": [Var, 3],
    "mean": [Mean, 3],
    "+": [Add, 2],
    "-": [Subtract, 2],
    "*": [Multiply, 2],
    "/": [Divide, 2],
    "negate": [Negate, 1],
    "pow": [Pow, 2],
    "log": [Log, 2]
}

const parse = parsable => {
    return parsable.split(" ").filter(token => token !== "").reduce(parse2, []).pop();
}

const parse2 = (parseStack, token) => {
    let intToken = parseInt(token);
    if (token in varNames) {
        parseStack.push(new Variable(token));
    } else if (token in oper) {
        let num = oper[token][1];
        let arr = parseStack.slice(parseStack.length - num);
        parseStack.splice(parseStack.length - num, arr.length, new oper[token][0](...arr));
    } else {
        parseStack.push(new Const(intToken));
    }
    return parseStack;
}


function CustomError(message) {
    Error.call(this, message);
    this.message = message;
}

CustomError.prototype = Object.create(Error.prototype);
CustomError.prototype.constructor = CustomError;
CustomError.prototype.name = "CustomError";

const parsePrefix = (parsable) => {
    return new Parser(parsable, "prefix").parse();
}

const parsePostfix = parsable => {
    return new Parser(parsable, "postfix").parse()
}

class Parser {
    pos = 0;
    savedState = 0;

    constructor(expression, type) {
        this.expression = expression;
        this.type = type;
    }

    #skipSpaces() {
        while (this.#next() && this.#check(" ")) {
            this.#movePos()
        }
    }

    #getChar() {
        return this.expression[this.pos];
    }

    #expect(expected) {
        if (!this.#check(expected)) {
            throw new CustomError("expected a closing bracket");
        }
        this.#movePos();
        return true;
    }

    #check(checked) {
        return checked === this.#getChar();
    }

    #next() {
        return this.pos < this.expression.length;
    }

    #movePos() {
        this.pos++;
    }

    #isOpenBracket() {
        return this.#getChar() === "("
    }

    #isCloseBracket() {
        return this.#getChar() === ")"
    }

    #isSpace() {
        return this.#getChar() === " "
    }

    #parseToken() {
        this.savedState = this.pos;
        let token = "";
        this.#skipSpaces();
        if (this.#isOpenBracket()) {
            token = this.#getChar();
            return token;
        }
        while (this.#next() && !this.#isSpace() && !this.#isCloseBracket() && !this.#isOpenBracket()) {
            token = token + this.#getChar();
            this.#movePos();
        }
        return token;
    }

    parse() {
        const ans = this.#parseExpression();
        this.#skipSpaces();
        if (this.#next()) {
            if (this.expression[0] !== "(") {
                throw new CustomError("missing ( at position " + this.pos);
            } else {
                throw new CustomError("too much info! at position " + this.pos);
            }
        }
        return ans;
    }

    #parseExpression() {
        this.#skipSpaces();
        if (this.#isOpenBracket()) {
            this.#movePos();
            let op, a;
            if (this.type === "prefix") {
                op = this.#parseOperation();
                a = this.#parseArguments();
            } else {
                a = this.#parseArguments();
                op = this.#parseOperation();
                this.#skipSpaces();
                this.#expect(")");
            }
            if (oper[op][1] < 3 && oper[op][1] !== a.length) {
                throw new CustomError("wrong amount of arguments at position " + this.pos);
            }
            return new oper[op][0](...a);
        }
        let token = this.#parseToken();
        if (token in varNames) {
            return new Variable(token);
        }
        if (Number.isInteger(parseInt(token))) {
            return new Const(parseInt(token));
        }
        throw new CustomError("no opening bracket? at position " + this.pos);
    }

    #parseOperation() {
        this.#skipSpaces();
        let token = this.#parseToken();
        if (token in oper) {
            return token;
        }
        if (this.type === "postfix") {
            this.#expect(")")
        }
        throw new CustomError("bad operation at position " + this.pos);
    }

    #rollBack() {
        this.pos = this.savedState;
    }

    #parseArguments() {
        let a = [];
        let token;
        this.#skipSpaces();
        while (this.#next && (token = this.#parseToken()) !== "") {
            if (token === "(") {
                a.push(this.#parseExpression());
            } else if (token in oper) {
                this.#rollBack();
                return a;
            } else if (token in varNames) {
                a.push(new Variable(token));
            } else if ((Number.parseInt(token)).toString() !== token) {
                throw new CustomError("what kind of symbol is that? at position " + this.pos);
            } else {
                a.push(new Const(parseInt(token)));
            }
            this.#skipSpaces()
        }
        this.#skipSpaces()
        if (this.type === "prefix") {
            this.#expect(")");
        }
        return a;
    }
}
