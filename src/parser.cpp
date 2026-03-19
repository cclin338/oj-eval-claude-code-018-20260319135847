/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion
 * 
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {
    return Expr(new RationalNum(numerator, denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }

    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {
        // If first element is not a symbol, treat as Apply expression
        // TODO: Implement proper Apply parsing
        throw RuntimeError("Unimplemented: Apply with non-symbol operator");
    }else{
    string op = id->s;
    if (find(op, env).get() != nullptr) {
        // Variable found in environment, treat as function application
        // TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        throw RuntimeError("Unimplemented: Variable function application");
    }
    if (primitives.count(op) != 0) {
        vector<Expr> parameters;
        // Parse parameters from the rest of the list
        for (size_t i = 1; i < stxs.size(); ++i) {
            parameters.push_back(stxs[i]->parse(env));
        }

        ExprType op_type = primitives[op];
        if (op_type == E_PLUS) {
            if (parameters.size() == 2) {
                return Expr(new Plus(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for +");
            }
        } else if (op_type == E_MINUS) {
            if (parameters.size() == 2) {
                return Expr(new Minus(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for -");
            }
        } else if (op_type == E_MUL) {
            if (parameters.size() == 2) {
                return Expr(new Mult(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for *");
            }
        }  else if (op_type == E_DIV) {
            if (parameters.size() == 2) {
                return Expr(new Div(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for /");
            }
        } else if (op_type == E_MODULO) {
            if (parameters.size() != 2) {
                throw RuntimeError("Wrong number of arguments for modulo");
            }
            return Expr(new Modulo(parameters[0], parameters[1]));
        } else if (op_type == E_LIST) {
            return Expr(new ListFunc(parameters));
        } else if (op_type == E_LT) {
            if (parameters.size() == 2) {
                return Expr(new Less(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for <");
            }
        } else if (op_type == E_LE) {
            if (parameters.size() == 2) {
                return Expr(new LessEq(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for <=");
            }
        } else if (op_type == E_EQ) {
            if (parameters.size() == 2) {
                return Expr(new Equal(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for =");
            }
        } else if (op_type == E_GE) {
            if (parameters.size() == 2) {
                return Expr(new GreaterEq(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for >=");
            }
        } else if (op_type == E_GT) {
            if (parameters.size() == 2) {
                return Expr(new Greater(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for >");
            }
        } else if (op_type == E_AND) {
            return Expr(new AndVar(parameters));
        } else if (op_type == E_OR) {
            return Expr(new OrVar(parameters));
        } else {
            // Handle other primitives (not, eq?, etc.)
            // TODO: Implement other primitives
            throw RuntimeError("Unimplemented primitive: " + op);
        }
    }

    if (reserved_words.count(op) != 0) {
    	switch (reserved_words[op]) {
            case E_QUOTE:
                if (stxs.size() != 2) {
                    throw RuntimeError("Wrong number of arguments for quote");
                }
                return Expr(new Quote(stxs[1]));
            // TODO: Implement other reserved words
            default:
                throw RuntimeError("Unimplemented reserved word: " + op);
    	}
    }

    //default: use Apply to be an expression
    //TODO: TO COMPLETE THE PARSER LOGIC
    // For now, return a placeholder
    throw RuntimeError("Unimplemented: Apply expression parsing");
}
}
