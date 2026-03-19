/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp" 
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    std::vector<Value> evaluated_rands;
    for (auto& rand_expr : rands) {
        evaluated_rands.push_back(rand_expr->eval(e));
    }
    return evalRator(evaluated_rands);
}

Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    
    Value matched_value = find(x, e);
    if (matched_value.get() == nullptr) {
        if (primitives.count(x)) {
             static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                    {E_VOID,     {new MakeVoid(), {}}},
                    {E_EXIT,     {new Exit(), {}}},
                    {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
                    {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
                    {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
                    {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
                    {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
                    {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
                    {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
                    {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},
                    {E_PLUS,     {new PlusVar({}),  {}}},
                    {E_MINUS,    {new MinusVar({}), {}}},
                    {E_MUL,      {new MultVar({}),  {}}},
                    {E_DIV,      {new DivVar({}),   {}}},
                    {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EQQ,      {new EqualVar({}), {}}},
            };

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
            }
      }
    }
    return matched_value;
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    // Handle Integer + Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // Check for overflow
        if ((n2 > 0 && n1 > INT_MAX - n2) || (n2 < 0 && n1 < INT_MIN - n2)) {
            throw RuntimeError("Integer overflow in addition");
        }
        return IntegerV(n1 + n2);
    }
    // Handle Rational + Integer
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // a/b + c = (a + c*b)/b
        int new_num = r1->numerator + n2 * r1->denominator;
        return RationalV(new_num, r1->denominator);
    }
    // Handle Integer + Rational
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // c + a/b = (c*b + a)/b
        int new_num = n1 * r2->denominator + r2->numerator;
        return RationalV(new_num, r2->denominator);
    }
    // Handle Rational + Rational
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // a/b + c/d = (a*d + c*b)/(b*d)
        int new_num = r1->numerator * r2->denominator + r2->numerator * r1->denominator;
        int new_den = r1->denominator * r2->denominator;
        return RationalV(new_num, new_den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    // Handle Integer - Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // Check for overflow
        if ((n2 > 0 && n1 < INT_MIN + n2) || (n2 < 0 && n1 > INT_MAX + n2)) {
            throw RuntimeError("Integer overflow in subtraction");
        }
        return IntegerV(n1 - n2);
    }
    // Handle Rational - Integer
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // a/b - c = (a - c*b)/b
        int new_num = r1->numerator - n2 * r1->denominator;
        return RationalV(new_num, r1->denominator);
    }
    // Handle Integer - Rational
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // c - a/b = (c*b - a)/b
        int new_num = n1 * r2->denominator - r2->numerator;
        return RationalV(new_num, r2->denominator);
    }
    // Handle Rational - Rational
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // a/b - c/d = (a*d - c*b)/(b*d)
        int new_num = r1->numerator * r2->denominator - r2->numerator * r1->denominator;
        int new_den = r1->denominator * r2->denominator;
        return RationalV(new_num, new_den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    // Handle Integer * Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // Check for overflow
        if (n1 > 0) {
            if (n2 > 0) {
                if (n1 > INT_MAX / n2) throw RuntimeError("Integer overflow in multiplication");
            } else if (n2 < 0) {
                if (n2 < INT_MIN / n1) throw RuntimeError("Integer overflow in multiplication");
            }
        } else if (n1 < 0) {
            if (n2 > 0) {
                if (n1 < INT_MIN / n2) throw RuntimeError("Integer overflow in multiplication");
            } else if (n2 < 0) {
                if (n1 < INT_MAX / n2) throw RuntimeError("Integer overflow in multiplication");
            }
        }
        return IntegerV(n1 * n2);
    }
    // Handle Rational * Integer
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // (a/b) * c = (a*c)/b
        int new_num = r1->numerator * n2;
        // Check for overflow in multiplication
        if (r1->numerator != 0 && n2 != 0 &&
            ((new_num / n2) != r1->numerator || (new_num / r1->numerator) != n2)) {
            throw RuntimeError("Integer overflow in multiplication");
        }
        return RationalV(new_num, r1->denominator);
    }
    // Handle Integer * Rational
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // c * (a/b) = (c*a)/b
        int new_num = n1 * r2->numerator;
        // Check for overflow
        if (n1 != 0 && r2->numerator != 0 &&
            ((new_num / n1) != r2->numerator || (new_num / r2->numerator) != n1)) {
            throw RuntimeError("Integer overflow in multiplication");
        }
        return RationalV(new_num, r2->denominator);
    }
    // Handle Rational * Rational
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // (a/b) * (c/d) = (a*c)/(b*d)
        int new_num = r1->numerator * r2->numerator;
        int new_den = r1->denominator * r2->denominator;
        // Check for overflow
        if (r1->numerator != 0 && r2->numerator != 0 &&
            ((new_num / r1->numerator) != r2->numerator || (new_num / r2->numerator) != r1->numerator)) {
            throw RuntimeError("Integer overflow in multiplication");
        }
        if (r1->denominator != 0 && r2->denominator != 0 &&
            ((new_den / r1->denominator) != r2->denominator || (new_den / r2->denominator) != r1->denominator)) {
            throw RuntimeError("Integer overflow in multiplication");
        }
        return RationalV(new_num, new_den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    // Check for division by zero
    if ((rand2->v_type == V_INT && dynamic_cast<Integer*>(rand2.get())->n == 0) ||
        (rand2->v_type == V_RATIONAL && dynamic_cast<Rational*>(rand2.get())->numerator == 0)) {
        throw RuntimeError("Division by zero");
    }

    // Handle Integer / Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // Integer division creates rational number
        return RationalV(n1, n2);
    }
    // Handle Rational / Integer
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        // (a/b) / c = a/(b*c)
        int new_den = r1->denominator * n2;
        // Check for overflow
        if (r1->denominator != 0 && n2 != 0 &&
            ((new_den / n2) != r1->denominator || (new_den / r1->denominator) != n2)) {
            throw RuntimeError("Integer overflow in division");
        }
        return RationalV(r1->numerator, new_den);
    }
    // Handle Integer / Rational
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // c / (a/b) = (c*b)/a
        int new_num = n1 * r2->denominator;
        // Check for overflow
        if (n1 != 0 && r2->denominator != 0 &&
            ((new_num / n1) != r2->denominator || (new_num / r2->denominator) != n1)) {
            throw RuntimeError("Integer overflow in division");
        }
        return RationalV(new_num, r2->numerator);
    }
    // Handle Rational / Rational
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        // (a/b) / (c/d) = (a*d)/(b*c)
        int new_num = r1->numerator * r2->denominator;
        int new_den = r1->denominator * r2->numerator;
        // Check for overflow
        if (r1->numerator != 0 && r2->denominator != 0 &&
            ((new_num / r1->numerator) != r2->denominator || (new_num / r2->denominator) != r1->numerator)) {
            throw RuntimeError("Integer overflow in division");
        }
        if (r1->denominator != 0 && r2->numerator != 0 &&
            ((new_den / r1->denominator) != r2->numerator || (new_den / r2->numerator) != r1->denominator)) {
            throw RuntimeError("Integer overflow in division");
        }
        return RationalV(new_num, new_den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    if (args.empty()) {
        return IntegerV(0); // (+) returns 0
    }

    if (args.size() == 1) {
        return args[0]; // (+ x) returns x
    }

    // Start with first argument
    Value result = args[0];

    // For each subsequent argument, add it to result
    for (size_t i = 1; i < args.size(); ++i) {
        const Value& current = args[i];

        // Check types and perform addition
        if (result->v_type == V_INT && current->v_type == V_INT) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            // Check for overflow
            if ((n2 > 0 && n1 > INT_MAX - n2) || (n2 < 0 && n1 < INT_MIN - n2)) {
                throw RuntimeError("Integer overflow in addition");
            }
            result = IntegerV(n1 + n2);
        }
        // Handle Rational + Integer
        else if (result->v_type == V_RATIONAL && current->v_type == V_INT) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            int new_num = r1->numerator + n2 * r1->denominator;
            result = RationalV(new_num, r1->denominator);
        }
        // Handle Integer + Rational
        else if (result->v_type == V_INT && current->v_type == V_RATIONAL) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = n1 * r2->denominator + r2->numerator;
            result = RationalV(new_num, r2->denominator);
        }
        // Handle Rational + Rational
        else if (result->v_type == V_RATIONAL && current->v_type == V_RATIONAL) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = r1->numerator * r2->denominator + r2->numerator * r1->denominator;
            int new_den = r1->denominator * r2->denominator;
            result = RationalV(new_num, new_den);
        }
        else {
            throw RuntimeError("Wrong typename in addition");
        }
    }

    return result;
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    if (args.empty()) {
        throw RuntimeError("- requires at least one argument");
    }

    if (args.size() == 1) {
        // (- x) returns -x
        const Value& arg = args[0];
        if (arg->v_type == V_INT) {
            int n = dynamic_cast<Integer*>(arg.get())->n;
            // Check for overflow when negating
            if (n == INT_MIN) {
                throw RuntimeError("Integer overflow in negation");
            }
            return IntegerV(-n);
        } else if (arg->v_type == V_RATIONAL) {
            Rational* r = dynamic_cast<Rational*>(arg.get());
            return RationalV(-r->numerator, r->denominator);
        } else {
            throw RuntimeError("Wrong typename in negation");
        }
    }

    // Start with first argument
    Value result = args[0];

    // For each subsequent argument, subtract it from result
    for (size_t i = 1; i < args.size(); ++i) {
        const Value& current = args[i];

        // Check types and perform subtraction using same logic as binary minus
        if (result->v_type == V_INT && current->v_type == V_INT) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            // Check for overflow
            if ((n2 > 0 && n1 < INT_MIN + n2) || (n2 < 0 && n1 > INT_MAX + n2)) {
                throw RuntimeError("Integer overflow in subtraction");
            }
            result = IntegerV(n1 - n2);
        }
        // Handle Rational - Integer
        else if (result->v_type == V_RATIONAL && current->v_type == V_INT) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            int new_num = r1->numerator - n2 * r1->denominator;
            result = RationalV(new_num, r1->denominator);
        }
        // Handle Integer - Rational
        else if (result->v_type == V_INT && current->v_type == V_RATIONAL) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = n1 * r2->denominator - r2->numerator;
            result = RationalV(new_num, r2->denominator);
        }
        // Handle Rational - Rational
        else if (result->v_type == V_RATIONAL && current->v_type == V_RATIONAL) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = r1->numerator * r2->denominator - r2->numerator * r1->denominator;
            int new_den = r1->denominator * r2->denominator;
            result = RationalV(new_num, new_den);
        }
        else {
            throw RuntimeError("Wrong typename in subtraction");
        }
    }

    return result;
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    if (args.empty()) {
        return IntegerV(1); // (*) returns 1
    }

    if (args.size() == 1) {
        return args[0]; // (* x) returns x
    }

    // Start with first argument
    Value result = args[0];

    // For each subsequent argument, multiply it with result
    for (size_t i = 1; i < args.size(); ++i) {
        const Value& current = args[i];

        // Check types and perform multiplication
        if (result->v_type == V_INT && current->v_type == V_INT) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            // Check for overflow
            if (n1 > 0) {
                if (n2 > 0) {
                    if (n1 > INT_MAX / n2) throw RuntimeError("Integer overflow in multiplication");
                } else if (n2 < 0) {
                    if (n2 < INT_MIN / n1) throw RuntimeError("Integer overflow in multiplication");
                }
            } else if (n1 < 0) {
                if (n2 > 0) {
                    if (n1 < INT_MIN / n2) throw RuntimeError("Integer overflow in multiplication");
                } else if (n2 < 0) {
                    if (n1 < INT_MAX / n2) throw RuntimeError("Integer overflow in multiplication");
                }
            }
            result = IntegerV(n1 * n2);
        }
        // Handle Rational * Integer
        else if (result->v_type == V_RATIONAL && current->v_type == V_INT) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            int new_num = r1->numerator * n2;
            // Check for overflow in multiplication
            if (r1->numerator != 0 && n2 != 0 &&
                ((new_num / n2) != r1->numerator || (new_num / r1->numerator) != n2)) {
                throw RuntimeError("Integer overflow in multiplication");
            }
            result = RationalV(new_num, r1->denominator);
        }
        // Handle Integer * Rational
        else if (result->v_type == V_INT && current->v_type == V_RATIONAL) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = n1 * r2->numerator;
            // Check for overflow
            if (n1 != 0 && r2->numerator != 0 &&
                ((new_num / n1) != r2->numerator || (new_num / r2->numerator) != n1)) {
                throw RuntimeError("Integer overflow in multiplication");
            }
            result = RationalV(new_num, r2->denominator);
        }
        // Handle Rational * Rational
        else if (result->v_type == V_RATIONAL && current->v_type == V_RATIONAL) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = r1->numerator * r2->numerator;
            int new_den = r1->denominator * r2->denominator;
            // Check for overflow
            if (r1->numerator != 0 && r2->numerator != 0 &&
                ((new_num / r1->numerator) != r2->numerator || (new_num / r2->numerator) != r1->numerator)) {
                throw RuntimeError("Integer overflow in multiplication");
            }
            if (r1->denominator != 0 && r2->denominator != 0 &&
                ((new_den / r1->denominator) != r2->denominator || (new_den / r2->denominator) != r1->denominator)) {
                throw RuntimeError("Integer overflow in multiplication");
            }
            result = RationalV(new_num, new_den);
        }
        else {
            throw RuntimeError("Wrong typename in multiplication");
        }
    }

    return result;
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    if (args.empty()) {
        throw RuntimeError("/ requires at least one argument");
    }

    if (args.size() == 1) {
        // (/ x) returns 1/x
        const Value& arg = args[0];
        // Check for division by zero
        if ((arg->v_type == V_INT && dynamic_cast<Integer*>(arg.get())->n == 0) ||
            (arg->v_type == V_RATIONAL && dynamic_cast<Rational*>(arg.get())->numerator == 0)) {
            throw RuntimeError("Division by zero");
        }

        if (arg->v_type == V_INT) {
            int n = dynamic_cast<Integer*>(arg.get())->n;
            return RationalV(1, n);
        } else if (arg->v_type == V_RATIONAL) {
            Rational* r = dynamic_cast<Rational*>(arg.get());
            return RationalV(r->denominator, r->numerator);
        } else {
            throw RuntimeError("Wrong typename in division");
        }
    }

    // Start with first argument
    Value result = args[0];

    // For each subsequent argument, divide result by it
    for (size_t i = 1; i < args.size(); ++i) {
        const Value& current = args[i];

        // Check for division by zero
        if ((current->v_type == V_INT && dynamic_cast<Integer*>(current.get())->n == 0) ||
            (current->v_type == V_RATIONAL && dynamic_cast<Rational*>(current.get())->numerator == 0)) {
            throw RuntimeError("Division by zero");
        }

        // Check types and perform division
        if (result->v_type == V_INT && current->v_type == V_INT) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            // Integer division creates rational number
            result = RationalV(n1, n2);
        }
        // Handle Rational / Integer
        else if (result->v_type == V_RATIONAL && current->v_type == V_INT) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            int n2 = dynamic_cast<Integer*>(current.get())->n;
            int new_den = r1->denominator * n2;
            // Check for overflow
            if (r1->denominator != 0 && n2 != 0 &&
                ((new_den / n2) != r1->denominator || (new_den / r1->denominator) != n2)) {
                throw RuntimeError("Integer overflow in division");
            }
            result = RationalV(r1->numerator, new_den);
        }
        // Handle Integer / Rational
        else if (result->v_type == V_INT && current->v_type == V_RATIONAL) {
            int n1 = dynamic_cast<Integer*>(result.get())->n;
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = n1 * r2->denominator;
            // Check for overflow
            if (n1 != 0 && r2->denominator != 0 &&
                ((new_num / n1) != r2->denominator || (new_num / r2->denominator) != n1)) {
                throw RuntimeError("Integer overflow in division");
            }
            result = RationalV(new_num, r2->numerator);
        }
        // Handle Rational / Rational
        else if (result->v_type == V_RATIONAL && current->v_type == V_RATIONAL) {
            Rational* r1 = dynamic_cast<Rational*>(result.get());
            Rational* r2 = dynamic_cast<Rational*>(current.get());
            int new_num = r1->numerator * r2->denominator;
            int new_den = r1->denominator * r2->numerator;
            // Check for overflow
            if (r1->numerator != 0 && r2->denominator != 0 &&
                ((new_num / r1->numerator) != r2->denominator || (new_num / r2->denominator) != r1->numerator)) {
                throw RuntimeError("Integer overflow in division");
            }
            if (r1->denominator != 0 && r2->numerator != 0 &&
                ((new_den / r1->denominator) != r2->numerator || (new_den / r2->numerator) != r1->denominator)) {
                throw RuntimeError("Integer overflow in division");
            }
            result = RationalV(new_num, new_den);
        }
        else {
            throw RuntimeError("Wrong typename in division");
        }
    }

    return result;
}

Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp < 0);
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp <= 0);
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp == 0);
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp >= 0);
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp > 0);
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    if (args.size() < 2) {
        throw RuntimeError("< requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        int cmp = compareNumericValues(args[i], args[i + 1]);
        if (cmp >= 0) { // Not strictly less
            return BooleanV(false);
        }
    }

    return BooleanV(true);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    if (args.size() < 2) {
        throw RuntimeError("<= requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        int cmp = compareNumericValues(args[i], args[i + 1]);
        if (cmp > 0) { // Not less or equal
            return BooleanV(false);
        }
    }

    return BooleanV(true);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    if (args.size() < 2) {
        throw RuntimeError("= requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        int cmp = compareNumericValues(args[i], args[i + 1]);
        if (cmp != 0) { // Not equal
            return BooleanV(false);
        }
    }

    return BooleanV(true);
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    if (args.size() < 2) {
        throw RuntimeError(">= requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        int cmp = compareNumericValues(args[i], args[i + 1]);
        if (cmp < 0) { // Not greater or equal
            return BooleanV(false);
        }
    }

    return BooleanV(true);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    if (args.size() < 2) {
        throw RuntimeError("> requires at least 2 arguments");
    }

    for (size_t i = 0; i < args.size() - 1; ++i) {
        int cmp = compareNumericValues(args[i], args[i + 1]);
        if (cmp <= 0) { // Not strictly greater
            return BooleanV(false);
        }
    }

    return BooleanV(true);
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    // Create a new pair with rand1 as car and rand2 as cdr
    return Value(new Pair(rand1, rand2));
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    // Build a list from right to left
    Value result = Value(new Null()); // Start with empty list

    // Iterate from last to first to build the list
    for (int i = args.size() - 1; i >= 0; --i) {
        result = Value(new Pair(args[i], result));
    }

    return result;
}

Value IsList::evalRator(const Value &rand) { // list?
    // Check if value is a proper list (ends with null)
    Value current = rand;
    while (current->v_type == V_PAIR) {
        Pair* pair = dynamic_cast<Pair*>(current.get());
        current = pair->cdr;
    }

    // Proper list ends with null
    return BooleanV(current->v_type == V_NULL);
}

Value Car::evalRator(const Value &rand) { // car
    if (rand->v_type != V_PAIR) {
        throw RuntimeError("car: argument must be a pair");
    }

    Pair* pair = dynamic_cast<Pair*>(rand.get());
    return pair->car;
}

Value Cdr::evalRator(const Value &rand) { // cdr
    if (rand->v_type != V_PAIR) {
        throw RuntimeError("cdr: argument must be a pair");
    }

    Pair* pair = dynamic_cast<Pair*>(rand.get());
    return pair->cdr;
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // Check if type is Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // Check if type is Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // Check if type is Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // Check if type is Null or Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
    //TODO: To complete the begin logic
}

Value Quote::eval(Assoc& e) {
    // Convert Syntax to Value
    SyntaxBase* syntax_ptr = s.get();

    // Handle Number
    if (auto* num = dynamic_cast<Number*>(syntax_ptr)) {
        return IntegerV(num->n);
    }
    // Handle RationalSyntax
    else if (auto* rat = dynamic_cast<RationalSyntax*>(syntax_ptr)) {
        return RationalV(rat->numerator, rat->denominator);
    }
    // Handle TrueSyntax
    else if (dynamic_cast<TrueSyntax*>(syntax_ptr)) {
        return BooleanV(true);
    }
    // Handle FalseSyntax
    else if (dynamic_cast<FalseSyntax*>(syntax_ptr)) {
        return BooleanV(false);
    }
    // Handle SymbolSyntax
    else if (auto* sym = dynamic_cast<SymbolSyntax*>(syntax_ptr)) {
        return Value(new Symbol(sym->s));
    }
    // Handle StringSyntax
    else if (auto* str = dynamic_cast<StringSyntax*>(syntax_ptr)) {
        return StringV(str->s);
    }
    // Handle List
    else if (auto* lst = dynamic_cast<List*>(syntax_ptr)) {
        // Convert list to pair chain
        // Empty list
        if (lst->stxs.empty()) {
            return Value(new Null());
        }

        // Build list from right to left
        Value result = Value(new Null()); // Start with empty list

        // Iterate from last to first
        for (int i = lst->stxs.size() - 1; i >= 0; --i) {
            // Recursively quote each element
            Quote quote_expr(lst->stxs[i]);
            Value elem_value = quote_expr.eval(e);
            result = Value(new Pair(elem_value, result));
        }

        return result;
    }

    throw RuntimeError("Unsupported syntax type in quote");
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
}

Value Not::evalRator(const Value &rand) { // not
    //TODO: To complete the not logic
}

Value If::eval(Assoc &e) {
    //TODO: To complete the if logic
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
}

Value Lambda::eval(Assoc &env) { 
    //TODO: To complete the lambda logic
}

Value Apply::eval(Assoc &e) {
    if (rator->eval(e)->v_type != V_PROC) {throw RuntimeError("Attempt to apply a non-procedure");}

    //TODO: TO COMPLETE THE CLOSURE LOGIC
    Procedure* clos_ptr = dynamic_cast<Procedure*>(rator->eval(e).get());
    if (!clos_ptr) throw RuntimeError("Failed to cast to Procedure");

    //TODO: TO COMPLETE THE ARGUMENT PARSER LOGIC
    std::vector<Value> args;
    // For now, just evaluate all arguments
    for (auto& arg_expr : rand) {
        args.push_back(arg_expr->eval(e));
    }

    if (args.size() != clos_ptr->parameters.size()) throw RuntimeError("Wrong number of arguments");

    //TODO: TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
    Assoc param_env = clos_ptr->env; // Start with closure's environment

    // Extend with parameter bindings
    for (size_t i = 0; i < args.size(); ++i) {
        param_env = extend(clos_ptr->parameters[i], args[i], param_env);
    }

    return clos_ptr->e->eval(param_env);
}

Value Define::eval(Assoc &env) {
    //TODO: To complete the define logic
}

Value Let::eval(Assoc &env) {
    //TODO: To complete the let logic
}

Value Letrec::eval(Assoc &env) {
    //TODO: To complete the letrec logic
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    
    return VoidV();
}
