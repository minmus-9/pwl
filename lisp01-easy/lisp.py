#!/usr/bin/env python3
##
## pwl - python with lisp, a collection of lisp evaluators for Python
##       https://github.com/minmus-9/pwl
## Copyright (C) 2025  Mark Hays (github:minmus-9)
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

"lisp.py - an easy recursive lisp in 500 lines"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import operator
import sys
import traceback

## {{{ lisp core

T = True
EL = object()


class Symbol(str):
    ...


SYMBOLS = {}  ## global symbol table


def symbol(s):
    assert s and type(s) is str  ## pylint: disable=unidiomatic-typecheck
    return SYMBOLS.setdefault(s, Symbol(s))  ## mildly wasteful


def syntaxcheck(boolean, msg):  ## acts sorta like assert
    if not boolean:
        raise SyntaxError(msg)


def listcheck(x):
    if not (isinstance(x, list) and x):
        if x is EL:
            raise TypeError("expected list, got ()")
        raise TypeError(f"expected list, got {x!r}")
    return x


def symcheck(x):
    if not isinstance(x, Symbol):
        raise TypeError(f"expected symbol, got {stringify(x)}")
    return x


class Environment(dict):
    def __init__(self, params, args, parent):
        super().__init__()
        self.parent = parent
        params = [] if params is EL else listcheck(params)
        args = [] if args is EL else listcheck(args)
        variadic = False
        while params:
            p, params = symcheck(params[0]), params[1:]
            if p is symbol("&"):
                variadic = True
            elif variadic:
                syntaxcheck(not params, "too many params after '&'")
                self[p] = args or EL
                return
            elif not args:
                raise SyntaxError("not enough args")
            else:
                self[p], args = args[0], args[1:]
        syntaxcheck(not variadic, "args end with '&'")
        syntaxcheck(not args, "too many args")

    def find(self, k):  ## NB caller ensures k is a Symbol
        d = self
        while d is not None:
            if k in d:
                return d
            d = d.parent
        raise NameError(k)


def op_cond(args, env):
    for predicate, consequent in args:
        if leval(predicate, env) is not EL:
            return leval(consequent, env)
    return EL


def op_define(args, env):
    sym, defn = args
    env[symcheck(sym)] = leval(defn, env)


def op_lambda(args, env):
    params, body = args
    if params is not EL:
        listcheck(params)
    return Lambda(params, body, env)


def op_quote(args, _):
    (x,) = args
    return x


def op_setbang(args, env):
    sym, defn = args
    env.find(symcheck(sym))[sym] = leval(defn, env)


def op_special(args, env):
    sym, defn = args
    lam = leval(defn, env)
    if not isinstance(lam, Lambda):
        raise TypeError(lam)
    lam.special = True
    env[symcheck(sym)] = lam


SPECIALS_ = Environment(EL, EL, None)
SPECIALS_.update(
    {
        symbol("cond"): op_cond,
        symbol("define"): op_define,
        symbol("lambda"): op_lambda,
        symbol("quote"): op_quote,
        symbol("set!"): op_setbang,
        symbol("special"): op_special,
    }
)
SPECIALS = Environment(EL, EL, SPECIALS_)  ## user modifies this


def op_atom(args, _):
    (x,) = args
    return T if ((x is T) or (x is EL) or isinstance(x, Symbol)) else EL


def op_car(args, _):
    (x,) = args
    return listcheck(x)[0]


def op_cdr(args, _):
    (x,) = args
    return EL if x is EL else (listcheck(x)[1:] or EL)  ## [1:] breaks rule


def op_cons(args, _):
    x, y = args
    return [x] + ([] if y is EL else y)


def op_div(args, _):
    x, y = args
    if isinstance(x, int) and isinstance(y, int):
        return x // y
    return x / y


def op_eq(args, _):
    x, y = args
    if (x is T) or (x is EL) or isinstance(x, Symbol):
        return T if x is y else EL
    return EL


def op_equal(args, _):
    x, y = args
    return T if x == y else EL  ## NB polymorphic!


def op_error(args, _):
    (x,) = args
    raise Exception(x)


def op_eval(args, env):
    (x,) = args
    if isinstance(x, str) and not isinstance(x, Symbol):
        l = []
        p = Parser(l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    return leval(x, env)


def op_exit(args, _):
    (x,) = args
    raise SystemExit(x if isinstance(x, int) else stringify(x))


def op_lt(args, _):
    x, y = args
    return T if x < y else EL


def op_nand(args, _):
    x, y = args
    return ~(x & y)


def op_print(args, _):
    n = 0 if args is EL else len(args)
    for i in range(n):
        sep = "" if i == n - 1 else " "
        print(stringify(args[i]), end=sep)
    print()


def op_setcarbang(args, _):
    l, x = args
    listcheck(l)[0] = x


def op_setcdrbang(args, _):
    l, x = args
    listcheck(l)[1:] = listcheck(x)


def op_tostring(args, _):
    (x,) = args
    return stringify(x)


GLOBALS_ = Environment(EL, EL, None)
GLOBALS_.update(
    {
        symbol("#t"): T,
        symbol(">string"): op_tostring,
        symbol("atom?"): op_atom,
        symbol("car"): op_car,
        symbol("cdr"): op_cdr,
        symbol("cons"): op_cons,
        symbol("div"): op_div,
        symbol("eq?"): op_eq,
        symbol("equal?"): op_equal,
        symbol("error"): op_error,
        symbol("eval"): op_eval,
        symbol("exit"): op_exit,
        symbol("lt?"): op_lt,
        symbol("mul"): lambda args, _: operator.mul(*args),
        symbol("nand"): op_nand,
        symbol("print"): op_print,
        symbol("set-car!"): op_setcarbang,
        symbol("set-cdr!"): op_setcdrbang,
        symbol("sub"): lambda args, _: operator.sub(*args),
    }
)
GLOBALS = Environment(EL, EL, GLOBALS_)  ## user modifies this


class Lambda:
    ## pylint: disable=too-few-public-methods

    special = False

    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, args, env):
        parent = env if self.special else self.env  ## specials are weird
        e = Environment(self.params, args, parent)
        return leval(self.body, e)

    def __str__(self):  ## this is called by stringify()
        return (
            "(lambda "
            + stringify(self.params)
            + " "
            + stringify(self.body)
            + ")"
        )


def stringify(x):
    if x is T:
        return "#t"
    if x is EL:
        return "()"
    if isinstance(x, (Lambda, Symbol, int, float)):  ## check Symbol here...
        return str(x)  ## NB Lambda has special __str__ logic
    if isinstance(x, str):  ## ... and str here
        return '"' + repr(x)[1:-1].replace('"', '\\"') + '"'
    if not isinstance(x, list):
        return repr(x)  ## python func
    n, ret = len(listcheck(x)), ""
    for i in range(n):
        sep = "" if i == n - 1 else " "
        ret += stringify(x[i]) + sep
    return "(" + ret + ")"


def leval(x, env=GLOBALS):
    if isinstance(x, Symbol):  ## test Symbol *before* str
        return env.find(x)[x]
    if (x is T) or (x is EL) or isinstance(x, (int, float, str)):
        return x
    sym, *args = listcheck(x)
    args = list(args)
    if isinstance(sym, Symbol):
        try:
            op = SPECIALS.find(sym)[sym]
        except NameError:
            proc = leval(sym, env)  ## look it up
        else:
            ret = op(args, env)
            return EL if ret is None else ret  ## lets specials return None
    else:
        proc = leval(listcheck(sym), env)  ## should be a list, eval it
    if not callable(proc):  ## python func or Lambda
        raise TypeError(proc)
    if not getattr(proc, "special", False):  ## lambdas get evaluated args
        args = [leval(arg, env) for arg in args]
    ret = proc(args or EL, env)
    return EL if ret is None else ret  ## lets procs return None


## }}}
## {{{ scanner and parser


class Scanner:
    T_SYM = "sym"
    T_INT = "int"
    T_REAL = "real"
    T_STR = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_EOF = "eof"

    S_SYM = "sym"  ## building symbol
    S_CMNT = "comment"  ## comment to eol
    S_STR = "string"  ## string to "
    S_BS = "backslash"  ## saw \ inside "

    ESC = {"t": "\t", "n": "\n", "r": "\r", '"': '"', "\\": "\\"}

    def __init__(self, callback):
        self.callback = callback
        self.token = ""
        self.state = self.S_SYM

    def feed(self, text):
        ## pylint: disable=too-many-branches
        if text is None:
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        while text:
            ch, text = text[0], text[1:]
            if self.state == self.S_CMNT:
                if ch in "\n\r":
                    self.state = self.S_SYM
            elif self.state == self.S_STR:
                if ch == '"':
                    self.state = self.S_SYM
                    self.push(self.T_STR)
                elif ch == "\\":
                    self.state = self.S_BS
                else:
                    self.token += ch
            elif self.state == self.S_BS:
                ch = self.ESC.get(ch)
                syntaxcheck(ch is not None, "bad escape")
                self.token += ch
                self.state = self.S_STR
            elif ch in " \n\t\r":
                self.push(self.T_SYM)
            elif ch == ";":
                self.state = self.S_CMNT
            elif ch == '"':
                syntaxcheck(not self.token, "quote is not a delimiter")
                self.state = self.S_STR
            elif ch == "(":
                self.push(self.T_SYM)
                self.push(self.T_LPAR)
            elif ch == ")":
                self.push(self.T_SYM)
                self.push(self.T_RPAR)
            else:
                self.token += ch

    def push(self, ttype):
        t, self.token = self.token, ""
        if ttype == self.T_SYM:
            if not t:
                return
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_REAL
                except:  ## pylint: disable=bare-except
                    pass  ## value error, range error
        self.callback(ttype, t)


class Parser:
    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def process_token(self, ttype, token):
        if ttype == self.scanner.T_SYM:
            self.add(symbol(token))
        elif ttype == self.scanner.T_LPAR:
            self.stack.append([])
        elif ttype == self.scanner.T_RPAR:
            syntaxcheck(self.stack, "too many ')'s")
            l = self.stack.pop() or EL
            if self.stack:
                self.add(l)
            else:
                self.callback(l)
        elif ttype in (
            self.scanner.T_INT,
            self.scanner.T_REAL,
            self.scanner.T_STR,
        ):
            self.add(token)
        elif ttype == self.scanner.T_EOF:
            syntaxcheck(not self.stack, "premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        syntaxcheck(self.stack, f"expected '(' got {stringify(x)}")
        self.stack[-1].append(x)


## }}}
## {{{ main repl


def repl(callback):
    try:
        import readline as _  ## pylint: disable=import-outside-toplevel
    except ImportError:
        pass

    ## pylint: disable=unused-variable
    p, rc, stop = Parser(callback), 0, False

    def feed(x):
        nonlocal p, rc, stop
        try:
            p.feed(x)
        except SystemExit as exc:
            stop, rc = True, exc.args[0]
        except SyntaxError:
            ## have to reset scanner/parser state
            p = Parser(callback)
            traceback.print_exception(*sys.exc_info())
        except:  ## pylint: disable=bare-except
            traceback.print_exception(*sys.exc_info())

    while not stop:
        try:
            line = input("lisp> ") + "\n"
        except (EOFError, KeyboardInterrupt):
            feed(None)
            break
        feed(line)
    print("\nbye")
    return rc


def main():
    def callback(sexpr):
        value = leval(sexpr)
        if value is not EL:
            print(stringify(value))

    loaded = False
    for filename in sys.argv[1:]:
        if filename == "-":
            loaded = False
            break
        with open(  ## pylint: disable=unspecified-encoding
            filename, "r"
        ) as fp:
            p = Parser(callback)
            p.feed(fp.read())
            p.feed(None)
        loaded = True
    if not loaded:
        raise SystemExit(repl(callback))


## }}}

if __name__ == "__main__":
    main()


## EOF
