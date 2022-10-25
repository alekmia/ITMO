#include "calc.h"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;
bool fold = false;
bool error = true;

enum class Op
{
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
    // error
    case Op::ERR: return 0;
    // unary
    case Op::NEG: return 1;
    case Op::SQRT: return 1;
    // binary
    case Op::SET: return 2;
    case Op::ADD: return 2;
    case Op::SUB: return 2;
    case Op::MUL: return 2;
    case Op::DIV: return 2;
    case Op::REM: return 2;
    case Op::POW: return 2;
    }
    return 0;
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

bool more_arguments(const std::string & line, int pointer)
{
    if (skip_ws(line, pointer) != line.size())
        return true;
    return false;
}

Op parse_op(const std::string & line, std::size_t & i)
{
    const auto rollback = [&i, &line](const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        --i; // a first digit is a part of op's argument
        return Op::SET;
    case '+':
        return Op::ADD;
    case '-':
        return Op::SUB;
    case '*':
        return Op::MUL;
    case '/':
        return Op::DIV;
    case '%':
        return Op::REM;
    case '_':
        return Op::NEG;
    case '^':
        return Op::POW;
    case 'S':
        switch (line[i++]) {
        case 'Q':
            switch (line[i++]) {
            case 'R':
                switch (line[i++]) {
                case 'T':
                    return Op::SQRT;
                default:
                    return rollback(4);
                }
            default:
                return rollback(3);
            }
        default:
            return rollback(2);
        }
    default:
        return rollback(1);
    }
}

double parse_arg(const std::string & line, std::size_t & i)
{
    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (integer) {
                res *= 10;
                res += line[i] - '0';
            }
            else {
                fraction /= 10;
                res += (line[i] - '0') * fraction;
            }
            ++i;
            ++count;
            break;
        case '.':
            integer = false;
            ++i;
            break;
        default:
            good = false;
            break;
        }
    }
    if (!good) {
        if (!fold || !std::isdigit(line[i - 1])) {
            std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
            error = false;
        }
        else if (!more_arguments(line, i)) {
            std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
            i++;
        }
    }
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
        error = false;
    }
    return res;
}

double unary(const double current, const Op op)
{
    switch (op) {
    case Op::NEG:
        return -current;
    case Op::SQRT:
        if (current > 0) {
            return std::sqrt(current);
        }
        else {
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            [[fallthrough]];
        }
    default:
        return current;
    }
}

bool binary(const Op op, double & left, const double right)
{
    switch (op) {
    case Op::SET:
        left = right;
        break;
    case Op::ADD:
        left = left + right;
        break;
    case Op::SUB:
        left = left - right;
        break;
    case Op::MUL:
        left = left * right;
        break;
    case Op::DIV:
        if (right != 0) {
            left = left / right;
        }
        else {
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return false;
        }
        break;
    case Op::REM:
        if (right != 0) {
            left = std::fmod(left, right);
        }
        else {
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return false;
        }
        break;
    case Op::POW:
        left = std::pow(left, right);
        break;
    default:
        return true;
    }
    return true;
}

Op full_parse_op(const std::string & line, std::size_t & i)
{
    bool maybe_fold = false;
    if (line[i] == '(') {
        maybe_fold = true;
        i++;
    }
    auto op = parse_op(line, i);
    if (maybe_fold) {
        if (op == Op::SET)
            std::cerr << "set operation in fold?" << std::endl;
        if (line[i++] == ')') {
            fold = true;
            if (arity(op) == 1)
                std::cerr << "unary operation in fold?" << std::endl;
        }
        else {
            i--;
            std::cerr << "\nits bad to lie like that..." << std::endl;
            op = Op::ERR;
        }
    }
    return op;
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    fold = false;
    error = true;

    auto op = full_parse_op(line, i);

    switch (arity(op)) {
    case 2: {
        double ans = current;
        do {
            i = skip_ws(line, i);
            auto old_i = i;
            auto arg = parse_arg(line, i);
            error = error && binary(op, ans, arg);
            if (i == old_i) {
                std::cerr << "No argument for a binary operation" << std::endl;
                break;
            }
        } while (fold && more_arguments(line, i));
        return error ? ans : current;
    }
    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(current, op);
    }
    default: break;
    }
    return current;
}
