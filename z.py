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

"z.py - recursive lisp"

## {{{ prologue


## pylint: disable=invalid-name,unbalanced-tuple-unpacking
## pylint: disable=too-many-lines
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


__all__ = (
    "EL",
    "T",
    "symbol",
    "is_empty_list",
    "is_true",
    "is_symbol",
    "is_atom",
    "eq",
    "Pair",
    "car",
    "cdr",
    "cons",
    "set_car",
    "set_cdr",
    "is_pair",
    "is_integer",
    "is_float",
    "is_number",
    "is_string",
    "ltype",
    "ListBuilder",
    "Stack",
    "Environment",
    "glbls",
    "glbl",
    "spcl",
    "Lambda",
    "is_lambda",
    "stringify",
    "leval",
    "Scanner",
    "Parser",
    "parse",
    "execute",
    "load",
    "unpack",
    "unary",
    "binary",
    "repl",
    "main",
)


## }}}
## {{{ basics


class LispError(Exception):
    ...


class S_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "<SENTINEL>"


SENTINEL = S_()
del S_


class EL_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "()"


EL = EL_()
del EL_


def is_empty_list(x):
    return x is EL


class T_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "#t"


T = T_()
del T_


def is_true(x):
    return x is T


class Symbol(str):
    ...


def is_symbol(x):
    return isinstance(x, Symbol)


class SymbolTable(dict):
    def symbol(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        if s not in self:
            self[s] = Symbol(s)
        return self[s]


symbol = SymbolTable().symbol
del SymbolTable


def is_atom(x):
    return is_symbol(x) or is_empty_list(x) or is_true(x)


def eq(x, y):
    return is_atom(x) and x is y


class Pair(list):
    def __init__(self, l, r):
        super().__init__()
        self[:] = l, r

    def car(self):
        return self[0]

    def cdr(self):
        return self[1]

    def set_car(self, value):
        self[0] = value
        return EL

    def set_cdr(self, value):
        self[1] = value
        return EL


def car(x):
    if not is_pair(x):
        raise TypeError(f"expected pair, got {x!r}")
    return x.car()


def cdr(x):
    if not is_pair(x):
        raise TypeError(f"expected pair, got {x!r}")
    return x.cdr()


def cons(x, y):
    return Pair(x, y)


def set_car(x, value):
    if not is_pair(x):
        raise TypeError(f"expected pair, got {x!r}")
    return x.set_car(value)


def set_cdr(x, value):
    if not is_pair(x):
        raise TypeError(f"expected pair, got {x!r}")
    return x.set_cdr(value)


def is_pair(x):
    return isinstance(x, Pair)


def is_integer(x):
    return isinstance(x, int)


def is_float(x):
    return isinstance(x, float)


def is_number(x):
    return isinstance(x, (int, float))


def is_string(x):
    return isinstance(x, str) and not isinstance(x, Symbol)


def ltype(x):
    ## pylint: disable=too-many-return-statements
    if is_empty_list(x):
        return symbol("()")
    if is_true(x):
        return symbol("#t")
    if is_symbol(x):
        return symbol("symbol")
    if is_pair(x):
        return symbol("pair")
    if is_integer(x):
        return symbol("integer")
    if is_float(x):
        return symbol("float")
    if is_string(x):
        return symbol("string")
    if is_lambda(x):
        return symbol("lambda")
    if callable(x):
        return symbol("primitive")
    return symbol("opaque")


## }}}
## {{{ list builder


class ListBuilder:
    def __init__(self):
        self.h = self.t = EL

    def adjoin(self, x):
        if is_empty_list(self.h):
            self.h = self.t = x
        else:
            set_cdr(self.t, x)
        return EL

    def append(self, x):
        node = cons(x, EL)
        if is_empty_list(self.h):
            self.h = node
        else:
            set_cdr(self.t, node)
        self.t = node
        return EL

    def extend(self, seq):
        if is_empty_list(seq):
            return EL
        while is_pair(seq):
            self.append(car(seq))
            seq = cdr(seq)
        if not is_empty_list(seq):
            self.adjoin(seq)
        return EL

    def get(self):
        return self.h


## }}}
## {{{ stack


class Stack:
    def __init__(self):
        self.s = EL

    def empty(self):
        return is_empty_list(self.s)

    def push(self, x):
        self.s = cons(x, self.s)
        return EL

    def pop(self):
        r = car(self.s)
        self.s = cdr(self.s)
        return r

    def top(self):
        return car(self.s)


## }}}
## {{{ environment


class Environment(dict):
    def __init__(self, params, args, parent):
        super().__init__()
        self.p = parent
        self.bind(params, args)

    def bind(self, params, args):
        pl, al = params, args

        def se(msg):
            raise SyntaxError(msg + f" at {pl} <= {al}")

        def te(msg):
            raise TypeError(msg + f" at {pl} <= {al}")

        variadic = False
        while not is_empty_list(params):
            p, params = car(params), cdr(params)
            if not is_symbol(p):
                te(f"expected symbol, got {p!r}")
            if eq(p, symbol("&")):
                variadic = True
            elif variadic:
                if not is_empty_list(params):
                    se("extra junk after '&'")
                self[p] = args
                return
            elif is_empty_list(args):
                te("not enough args")
            else:
                a, args = car(args), cdr(args)
                self[p] = a
        if variadic:
            se("'&' ends param list")
        if not is_empty_list(args):
            te("too many args")

    def get(self, key, default):
        if not is_symbol(key):
            raise TypeError(f"expected symbol, got {key!r}")
        e = self
        while not is_empty_list(e):
            try:
                return e[key]
            except KeyError:
                e = e.p
        return default

    def setbang(self, key, value):
        if not is_symbol(key):
            raise TypeError(f"expected symbol, got {key!r}")
        e = self
        while not is_empty_list(e):
            if key in e:
                e[key] = value
                return
            e = e.p
        raise NameError(key)


## }}}
## {{{ globals


glbls = Environment(EL, EL, EL)
glbls[symbol("#t")] = T


def glbl(name):
    def wrap(func):
        glbls[name] = func
        return func

    return wrap


def spcl(name):
    def wrap(func):
        glbls[name] = func
        func.special = True
        return func

    return wrap


## }}}
## {{{ lambda


class Lambda:
    special = False

    def __init__(self, params, body, env):
        self.p, self.b, self.e = params, body, env

    def set_special(self):
        self.special = True

    def __call__(self, args, e):
        p = e if self.special else self.e
        e = Environment(self.p, args, p)
        return leval(self.b, e)

    def __str__(self):
        return "(lambda " + stringify(self.p) + " " + stringify(self.b) + ")"


def is_lambda(x):
    return isinstance(x, Lambda)


## }}}
## {{{ stringify


def stringify(x):
    if is_empty_list(x) or is_true(x):
        return repr(x)
    if is_symbol(x) or is_number(x) or is_string(x):
        return str(x)
    if not is_pair(x):
        if is_lambda(x):
            return str(x)
        if callable(x):
            return "[primitive]"
        return "[opaque]"
    parts = []
    while is_pair(x):
        parts.append(stringify(car(x)))
        x = cdr(x)
    if not is_empty_list(x):
        parts.append(".")
        parts.append(stringify(x))
    return "(" + " ".join(parts) + ")"


## }}}
## {{{ eval


def leval(x, e=None):
    e = glbls if e is None else e
    if is_symbol(x):
        obj = e.get(x, SENTINEL)
        if obj is SENTINEL:
            raise NameError(x)
        return obj
    if is_pair(x):
        sym, args = car(x), cdr(x)
    elif is_lambda(x):
        sym, args = x, EL
    else:
        return x
    if is_symbol(sym):
        op = e.get(sym, SENTINEL)
        if op is not SENTINEL and getattr(op, "special", False):
            return op(args, e)
        proc = leval(sym, e)
    elif callable(sym):
        proc = sym
    elif not is_pair(sym):
        raise TypeError(f"expected proc/list, got {sym!r}")
    else:
        proc = leval(sym, e)

    lb = ListBuilder()
    while is_pair(args):
        lb.append(leval(car(args), e))
        args = cdr(args)
    if not is_empty_list(args):
        lb.adjoin(args)
    return proc(lb.get(), e)


## }}}
## {{{ scanner and parser


class Scanner:
    T_SYM = "sym"
    T_INT = "int"
    T_REAL = "real"
    T_STR = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_EOF = "eof"

    S_SYM = "sym"  ## building symbol
    S_CMNT = "comment"  ## comment to eol
    S_STR = "string"  ## string to "
    S_BS = "backslash"  ## saw \ inside "
    S_COMMA = ","  ## saw ,

    ESC = {"t": "\t", "n": "\n", "r": "\r", '"': '"', "\\": "\\"}

    DELIM_LUT = {"(": ")", "[": "]"}

    def __init__(self, callback):
        self.callback = callback
        self.token = ""
        self.state = self.S_SYM
        self.stack = EL

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            if not is_empty_list(self.stack):
                raise SyntaxError(f"eof in {car(self.stack)!r}")
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
                c = self.ESC.get(ch)
                if c is None:
                    raise SyntaxError("bad escape {ch!r}")
                self.token += c
                self.state = self.S_STR
            elif self.state == self.S_COMMA:
                if ch == "@":
                    self.push(self.T_COMMA_AT)
                else:
                    self.push(self.T_COMMA)
                    text = ch + text
                self.state = self.S_SYM
            elif ch in " \n\t\r":
                self.push(self.T_SYM)
            elif ch == ";":
                self.state = self.S_CMNT
            elif ch == "'":
                if self.token:
                    raise SyntaxError("tick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_TICK)
            elif ch == "`":
                if self.token:
                    raise SyntaxError("backtick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_BACKTICK)
            elif ch == ",":
                if self.token:
                    raise SyntaxError("comma is not a delimiter")
                self.state = self.S_COMMA
            elif ch == '"':
                if self.token:
                    raise SyntaxError("quote is not a delimiter")
                self.state = self.S_STR
            elif ch in "([":
                self.stack = cons(symbol(self.DELIM_LUT[ch]), self.stack)
                self.push(self.T_SYM)
                self.push(self.T_LPAR)
            elif ch in ")]":
                if is_empty_list(self.stack):
                    raise SyntaxError(f"too many {ch!r}")
                c = car(self.stack)
                self.stack = cdr(self.stack)
                if not eq(c, symbol(ch)):
                    raise SyntaxError(f"expected {c!r}, got {ch!r}")
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
        self.stack = Stack()
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.q_map = {  ## could if-away these special cases but just use dict
            symbol("'"): symbol("quote"),
            symbol("`"): symbol("quasiquote"),
            symbol(","): symbol("unquote"),
            symbol(",@"): symbol("unquote-splicing"),
        }

    def process_token(self, ttype, token):
        ## pylint: disable=too-many-branches
        ## ugly, but the quickest to write
        if ttype == self.scanner.T_SYM:
            self.add(symbol(token))
        elif ttype == self.scanner.T_LPAR:
            self.stack.push(ListBuilder())
        elif ttype == self.scanner.T_RPAR:
            if self.stack.empty():
                raise SyntaxError("too many ')'s")
            lb = self.stack.pop()
            l = self.filter(lb.get())
            if self.stack.empty():
                self.callback(l)
            else:
                self.add(l)
        elif ttype in (
            self.scanner.T_INT,
            self.scanner.T_REAL,
            self.scanner.T_STR,
        ):
            self.add(token)
        elif ttype == self.scanner.T_TICK:
            self.add(symbol("'"))
        elif ttype == self.scanner.T_BACKTICK:
            self.add(symbol("`"))
        elif ttype == self.scanner.T_COMMA:
            self.add(symbol(","))
        elif ttype == self.scanner.T_COMMA_AT:
            self.add(symbol(",@"))
        elif ttype == self.scanner.T_EOF:
            if not self.stack.empty():
                raise SyntaxError("premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        if self.stack.empty():
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack.top().append(x)

    def filter(self, sexpr):
        ## pylint: disable=no-self-use
        "process ' ` , ,@"
        lb = ListBuilder()

        ## NB we know this is a well-formed list
        while not is_empty_list(sexpr):
            elt, sexpr = car(sexpr), cdr(sexpr)
            if is_symbol(elt) and elt in self.q_map:
                elt, sexpr = self.process_syms(elt, sexpr)
            lb.append(elt)
        return lb.get()

    def process_syms(self, elt, sexpr):
        replacement = self.q_map[elt]
        if is_empty_list(sexpr):
            raise SyntaxError(f"got {elt!r} at end of list")
        quoted, sexpr = car(sexpr), cdr(sexpr)
        if not (is_symbol(quoted) and quoted in self.q_map):
            elt = cons(replacement, cons(quoted, EL))
        else:
            quoted, sexpr = self.process_syms(quoted, sexpr)
            elt = cons(replacement, cons(quoted, EL))
        return elt, sexpr


def parse(text, callback):
    p = Parser(callback)
    p.feed(text)
    p.feed(None)


def execute(text):
    results = []

    def callback(sexpr):
        results.append(leval(sexpr))

    parse(text, callback)
    return results


def load(filename, callback=None):
    if os.path.isabs(filename):
        path = filename
    else:
        for d in [os.path.dirname(__file__)] + sys.path:
            path = os.path.join(d, filename)
            if os.path.isfile(path):
                break
        else:
            raise FileNotFoundError(filename)
    with open(path, "r", encoding=locale.getpreferredencoding()) as fp:
        if callback:
            parse(fp.read(), callback)
        else:
            execute(fp.read())


## }}}
## {{{ unpack


def unpack(args, n):
    al = args
    ret = []
    for _ in range(n):
        if is_empty_list(args):
            raise TypeError(f"not enough args, need {n} from {al!r}")
        if not is_pair(args):
            raise TypeError(f"malformed args, need {n} from {al!r}")
        ret.append(car(args))
        args = cdr(args)
    if not is_empty_list(args):
        raise TypeError(f"too many args, need {n} from {al!r}")
    return ret


## }}}
## {{{ special forms


@spcl("define")
def op_define(args, e):
    name, value = unpack(args, 2)
    if not is_symbol(name):
        raise TypeError(f"expected symbol, got {name!r}")
    e[name] = leval(value, e)
    return EL


@spcl("if")
def op_if(args, e):
    p, c, a = unpack(args, 3)
    if is_empty_list(leval(p, e)):
        return leval(a, e)
    return leval(c, e)


@spcl("lambda")
def op_lambda(args, e):
    params, body = unpack(args, 2)
    return Lambda(params, body, e)


@spcl("quote")
def op_quote(args, _):
    (x,) = unpack(args, 1)
    return x


@spcl("set!")
def op_setbang(args, e):
    name, value = unpack(args, 2)
    if not is_symbol(name):
        raise TypeError(f"expected symbol, got {name!r}")
    e.setbang(name, leval(value, e))
    return EL


@spcl("special")
def op_special(args, e):
    name, value = unpack(args, 2)
    if not is_symbol(name):
        raise TypeError(f"expected symbol, got {name!r}")
    value = leval(value, e)
    value.special = True  ## pylint: disable=attribute-defined-outside-init
    e[name] = value
    return EL


@spcl("trap")
def op_trap(args, e):
    (x,) = unpack(args, 1)
    ok = T
    try:
        res = leval(x, e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return cons(ok, cons(res, EL))


## }}}
## {{{ quasiquote


QQ = Stack()


def qq_list(form, e):
    lb = ListBuilder()
    while is_pair(form):
        elt, form = car(form), cdr(form)
        if is_pair(elt) and eq(car(elt), symbol("unquote-splicing")):
            _, x = unpack(elt, 2)
            lb.extend(leval(x, e))
        else:
            lb.append(qq(elt, e))
    return lb.get()


def qq_pair(form, e):
    h = car(form)
    if eq(h, symbol("quasiquote")):
        _, x = unpack(form, 2)
        return qq(x, e)
    if eq(h, symbol("unquote")):
        _, x = unpack(form, 2)
        return leval(x, e)
    if eq(h, symbol("unquote-splicing")):
        _, x = unpack(form, 2)
        raise LispError("cannot unquote-splicing here")
    return qq_list(form, e)


def qq(form, e):
    return qq_pair(form, e) if is_pair(form) else form


@spcl("quasiquote")
def op_quasiquote(args, e):
    (form,) = unpack(args, 1)
    return qq(form, e)


## }}}
## {{{ procedures


def unary(args, f):
    (x,) = unpack(args, 1)
    return f(x)


def binary(args, f):
    x, y = unpack(args, 2)
    return f(x, y)


@glbl("add")
def op_add(args, _):
    return binary(args, lambda x, y: x + y)


@glbl("atom?")
def op_atom(args, _):
    (x,) = unpack(args, 1)
    return T if is_atom(x) else EL


@glbl("car")
def op_car(args, _):
    return unary(args, car)


@glbl("cdr")
def op_cdr(args, _):
    return unary(args, cdr)


@glbl("cons")
def op_cons(args, _):
    return binary(args, cons)


@glbl("div")
def op_div(args, _):
    def f(x, y):
        if is_integer(x) and is_integer(y):
            return x // y
        return x / y

    return binary(args, f)


@glbl("eq?")
def op_eq(args, _):
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(args, f)


@glbl("equal?")
def op_equal(args, _):
    def f(x, y):
        return T if x == y else EL

    return binary(args, f)


@glbl("error")
def op_error(args, e):
    (x,) = unpack(args, 1)
    raise LispError(leval(x, e))


@glbl("eval")
def op_eval(args, e):
    try:
        x, n_up = unpack(args, 2)
    except TypeError:
        (x,) = unpack(args, 1)
        n_up = 0

    if is_string(x):
        l = []
        p = Parser(l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    for _ in range(n_up):
        if is_empty_list(e):
            raise ValueError(f"cannot go up {n_up} levels")
        e = e.p
    return leval(x, e)


@glbl("exit")
def op_exit(args, _):
    (x,) = unpack(args, 1)
    if is_integer(x):
        raise SystemExit(x)
    raise SystemExit(stringify(x))


@glbl("lt?")
def op_lt(args, _):
    def f(x, y):
        return T if x < y else EL

    return binary(args, f)


@glbl("mul")
def op_mul(args, _):
    return binary(args, lambda x, y: x * y)


@glbl("nand")
def op_nand(args, _):
    def f(x, y):
        return ~(x & y)

    return binary(args, f)


@glbl("print")
def op_print(args, _):
    if is_empty_list(args):
        print()
        return EL
    end = " "
    while is_pair(args):
        x, args = car(args), cdr(args)
        if is_empty_list(args):
            end = "\n"
        print(stringify(x), end=end)
    if not is_empty_list(args):
        print(".", stringify(x))
    return EL


@glbl("set-car!")
def op_setcar(args, _):
    return binary(args, set_car)


@glbl("set-cdr!")
def op_setcdr(args, _):
    return binary(args, set_cdr)


@glbl("sub")
def op_sub(args, _):
    return binary(args, lambda x, y: x - y)


@glbl("type")
def op_type(args, _):
    return unary(args, ltype)


@glbl("while")
def op_while(args, e):
    (x,) = unpack(args, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    while not is_empty_list(leval(x, e)):
        pass
    return EL


## }}}
## {{{ repl and main


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
        except:  ## pylint: disable=bare-except
            p = Parser(callback)
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


def main(force_repl=False):
    try:
        sys.set_int_max_str_digits(0)
    except AttributeError:
        pass

    def callback(sexpr):
        try:
            value = leval(sexpr)
        except SystemExit:
            raise
        except:
            print("Offender (pyth):", sexpr)
            print("Offender (lisp):", stringify(sexpr), "\n")
            raise
        if not is_empty_list(value):
            print(stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "+":
            continue  ## ignore for compatibility
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if force_repl or not stop:
        raise SystemExit(repl(callback))


## }}}


if __name__ == "__main__":
    main()


## EOF
