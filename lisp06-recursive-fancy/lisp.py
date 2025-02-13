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

"""
lisp.py -- fast recursive lisp with ffi and quasiquote, builds on
lisp05/lisp.py
"""

## pylint: disable=invalid-name,unbalanced-tuple-unpacking,too-many-lines
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


## {{{ basics


class error(Exception):
    pass


SENTINEL = object()
EL = object()
T = True


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["s"]

    def __init__(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        self.s = s

    def __str__(self):
        return self.s


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


SYMTAB = {}


def symbol(s):
    return SYMTAB.setdefault(s, Symbol(s))


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return is_atom(x) and x is y


def listcheck(x):
    if isinstance(x, list):
        return x
    if x is EL:
        raise TypeError("expected list, got ()")
    raise TypeError(f"expected list, got {x!r}")


def car(x):
    return listcheck(x)[0]


def cdr(x):
    return EL if x is EL else listcheck(x)[1]


def cons(x, y):
    if y is not EL:
        listcheck(y)
    return [x, y]


def set_car(x, y):
    listcheck(x)[0] = y


def set_cdr(x, y):
    if y is not EL:
        listcheck(y)
    x[1] = y


def splitcar(x):
    return listcheck(x)[0], x[1]


## }}}
## {{{ scanner


class Scanner:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.lut = {
            "(": self.c_lpar,
            ")": self.c_rpar,
            " ": self.c_ws,
            "\n": self.c_ws,
            "\r": self.c_ws,
            "\t": self.c_ws,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_semi,
            "'": self.c_tick,
            '"': self.c_quote,
            ",": self.c_comma,
            "`": self.c_backtick,
        }
        self.parens = []
        self.cont = self.k_sym
        self.callback = callback
        ## XXX ascii-specific
        for i in range(256):
            self.lut.setdefault(chr(i), self.token.append)

    def feed(self, text):
        if text is None:
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
        else:
            self.pos, n = 0, len(text)
            cont = self.cont
            while self.pos < n:
                p = self.pos
                ch = text[p]
                self.pos = p + 1
                cont = cont(ch) or cont
            self.cont = cont

    def push(self, ttype):
        l = self.token
        if l:
            t = "".join(l)
            l.clear()
        elif ttype == self.T_SYM:
            return
        else:
            self.callback(ttype, None)
            return
        if ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        self.callback(ttype, t)

    def k_sym(self, ch):
        return self.lut[ch](ch)

    def k_comment(self, ch):
        return self.k_sym if ch in "\n\r" else self.k_comment

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == "\"":
            self.push(self.T_STRING)
            return self.k_sym
        self.token.append(ch)
        return self.k_quote

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def k_backslash(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.token.append(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.token.append("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_quote(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        return self.k_quote

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(",")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(ch)
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(ch)
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rpar(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '['")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rbrack(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '('")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}
## {{{ parser


class Parser:
    Q_MAP = {
        "'": symbol("quote"),
        "`": symbol("quasiquote"),
        ",": symbol("unquote"),
        ",@": symbol("unquote-splicing"),
    }

    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.lut = {
            self.scanner.T_SYM: self.t_sym,
            self.scanner.T_INT: self.add,
            self.scanner.T_FLOAT: self.add,
            self.scanner.T_STRING: self.add,
            self.scanner.T_TICK: self.set_up_quote,
            self.scanner.T_BACKTICK: self.set_up_quote,
            self.scanner.T_COMMA: self.set_up_quote,
            self.scanner.T_COMMA_AT: self.set_up_quote,
            self.scanner.T_LPAR: self.t_lpar,
            self.scanner.T_RPAR: self.t_rpar,
            self.scanner.T_EOF: self.t_eof,
        }

    def t_sym(self, token):
        self.add(symbol(token))

    def t_lpar(self, _):
        self.qstack.append(")")
        self.stack.append(Queue())

    def t_rpar(self, _):
        assert self.stack  ## Scanner checks this
        assert self.qstack.pop() == ")"
        l = self.quote_wrap(self.stack.pop().head())
        if not self.stack:
            self.callback(l)
        else:
            self.add(l)

    def t_eof(self, _):
        assert not self.stack  ## Scanner checks this
        if self.qstack:
            raise SyntaxError("unclosed quasiquote")

    def process_token(self, ttype, token):
        self.lut[ttype](token)

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        ret = x
        while self.qstack and isinstance(self.qstack[-1], Symbol):
            s = self.qstack.pop()
            ret = cons(s, cons(ret, EL))
        return ret

    def set_up_quote(self, s):
        s = self.Q_MAP[s]
        self.qstack.append(s)


## }}}
## {{{ high level parsing routines


def parse(text, callback):
    p = Parser(callback)
    p.feed(text)
    p.feed(None)


def load(filename, callback):
    if os.path.isabs(filename):
        path = filename
    else:
        for d in ["", os.path.dirname(__file__)] + sys.path:
            path = os.path.join(d, filename)
            if os.path.isfile(path):
                break
        else:
            raise FileNotFoundError(filename)
    with open(path, "r", encoding=locale.getpreferredencoding()) as fp:
        parse(fp.read(), callback)


## }}}
## {{{ environment and globals


class Environment:
    __slots__ = ["p", "t"]

    def __init__(self, params, args, parent):
        self.p = parent
        assert isinstance(parent, Environment) or parent is SENTINEL
        self.t = {}
        variadic = False
        while params is not EL:
            param, params = splitcar(params)
            symcheck(param)
            if eq(param, symbol("&")):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                self.t[param] = args
                return
            elif args is EL:
                raise SyntaxError(f"not enough args {param!s}")
            else:
                arg, args = splitcar(args)
                self.t[param] = arg
        if args is not EL:
            raise SyntaxError("too many args")
        if variadic:
            raise SyntaxError("params end with '&'")

    def get(self, sym, default):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            x = e.t.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        return default

    def set(self, sym, value):
        self.t[symcheck(sym)] = value
        return EL

    def setbang(self, sym, value):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            if sym in e.t:
                e.t[sym] = value
                return EL
            e = e.p
        raise NameError(str(sym))


GLB = Environment(EL, EL, SENTINEL)
GLB.set(symbol("#t"), T)


def ffi(name):
    def wrap(func):
        GLB.set(symbol(name), func)
        func.ffi = True
        return func

    return wrap


def glbl(name):
    def wrap(func):
        GLB.set(symbol(name), func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        GLB.set(symbol(name), func)
        func.special = True
        return func

    return wrap


## }}}
## {{{ queue


class Queue:
    __slots__ = ["h", "t"]

    def __init__(self):
        self.h = self.t = EL

    def enqueue(self, x):
        n = cons(x, EL)
        if self.h is EL:
            self.h = n
        else:
            set_cdr(self.t, n)
        self.t = n

    def dequeue(self):
        n = self.h
        if n is EL:
            raise ValueError("queue empty")
        self.h = cdr(n)
        if self.h is EL:
            self.t = EL
        return car(n)

    def head(self):
        return self.h


## }}}
## {{{ lambda


class Lambda:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["p", "b", "e", "special"]

    def __init__(self, params, body, env):
        self.p, self.b, self.e = params, body, env
        self.special = False

    def __call__(self, args, e):
        p = e if self.special else self.e
        e = Environment(self.p, args, p)
        return leval(self.b, e)

    def __str__(self):
        return "(lambda " + stringify(self.p) + " " + stringify(self.b) + ")"


## }}}
## {{{ stringify


def stringify(x):
    ## pylint: disable=too-many-return-statements
    if x is EL:
        return "()"
    if x is T:
        return "#t"
    if isinstance(x, (Symbol, int, float, str)):
        return str(x)
    if not isinstance(x, list):
        if isinstance(x, Lambda):
            return str(x)
        if callable(x):
            return "[primitive]"
        return "[opaque]"
    parts = []
    while x is not EL:
        y, x = splitcar(x)
        parts.append(stringify(y))
    return "(" + " ".join(parts) + ")"


## }}}
## {{{ eval


def leval(x, e=SENTINEL):
    ## pylint: disable=too-many-branches
    e = GLB if e is SENTINEL else e
    if isinstance(x, Symbol):
        obj = e.get(x, SENTINEL)
        if obj is SENTINEL:
            raise NameError(x)
        return obj
    if isinstance(x, list):
        sym, args = splitcar(x)
    elif isinstance(x, Lambda):
        sym = x
        args = EL
    else:
        return x
    if isinstance(sym, Symbol):
        op = e.get(sym, SENTINEL)
        if op is not SENTINEL and getattr(op, "special", False):
            return op(args, e)
        proc = leval(sym, e)
    elif callable(sym):
        proc = sym
    elif not isinstance(sym, list):
        raise TypeError(f"expected proc/list, got {sym!r}")
    else:
        proc = leval(sym, e)
        assert proc is not True
    if not callable(proc):
        raise TypeError(f"expected proc, got {proc!r}")

    q = Queue()
    while args is not EL:
        arg, args = splitcar(args)
        q.enqueue(leval(arg, e))
    if getattr(proc, "ffi", False):
        return do_ffi(proc, q.head(), e)
    return proc(q.head(), e)


## }}}
## {{{ ffi support


def do_ffi(proc, args, e):
    return py_to_lisp(proc(lisp_to_py(args), e))


def lisp_to_py(args):
    ret = []
    while args is not EL:
        x, args = splitcar(args)
        if x is EL:
            ret.append(None)
        elif x is T:
            ret.append(True)
        elif not isinstance(x, list):
            ret.append(x)
        else:
            ret.append(lisp_to_py(x))
    return ret


def py_to_lisp(x):
    if x is None:
        return EL
    if x is True:
        return T
    if not isinstance(x, (list, tuple)):
        return x
    q = Queue()
    for z in x:
        q.enqueue(py_to_lisp(z))
    return q.head()


## }}}
## {{{ unpack


def unpack(args, n):
    ret = []
    for _ in range(n):
        if args is EL:
            raise TypeError(f"not enough args, need {n}")
        arg, args = splitcar(args)
        ret.append(arg)
    if args is not EL:
        raise TypeError(f"too many args, need {n}")
    return ret


## }}}
## {{{ special forms


@spcl("cond")
def op_cond(args, e):
    while args is not EL:
        arg, args = splitcar(args)
        predicate, consequent = unpack(arg, 2)
        if leval(predicate, e) is not EL:
            return leval(consequent, e)
    return EL


@spcl("define")
def op_define(args, e):
    name, value = unpack(args, 2)
    return e.set(symcheck(name), leval(value, e))


@spcl("if")
def op_if(args, e):
    p, c, a = unpack(args, 3)
    return leval(a, e) if leval(p, e) is EL else leval(c, e)


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
    e.setbang(symcheck(name), leval(value, e))
    return EL


@spcl("special")
def op_special(args, e):
    name, value = unpack(args, 2)
    value = leval(value, e)
    value.special = True
    return e.set(symcheck(name), value)


## }}}
## {{{ quasiquote


def qq_list(form, e):
    q = Queue()
    while form is not EL:
        elt, form = splitcar(form)
        if isinstance(elt, list) and eq(car(elt), symbol("unquote-splicing")):
            _, x = unpack(elt, 2)
            y = leval(x, e)
            if y is not EL:
                listcheck(y)
                while y is not EL:
                    z, y = splitcar(y)
                    q.enqueue(z)
        else:
            q.enqueue(qq(elt, e))
    return q.head()


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
        raise ValueError("cannot unquote-splicing here")
    return qq_list(form, e)


def qq(form, e):
    return qq_pair(form, e) if isinstance(form, list) else form


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
        if isinstance(x, int) and isinstance(y, int):
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
    raise error(leval(x, e))


@glbl("eval")
def op_eval(args, e):
    try:
        x, n_up = unpack(args, 2)
    except TypeError:
        (x,) = unpack(args, 1)
        n_up = 0
    if isinstance(x, str):
        l = []
        parse(x, lambda expr: l.append(leval(expr, e)))
        x = l[-1] if l else EL
    for _ in range(n_up):
        e = e.p
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
    return leval(x, e)


@glbl("exit")
def op_exit(args, _):
    (x,) = unpack(args, 1)
    if isinstance(x, int):
        raise SystemExit(x)
    raise SystemExit(stringify(x))


@glbl("last")
def op_last(args, _):
    (x,) = unpack(args, 1)
    ret = EL
    while x is not EL:
        ret, x = splitcar(x)
    return ret


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


@glbl("null?")
def op_null(args, _):
    (x,) = unpack(args, 1)
    return T if x is EL else EL


@glbl("print")
def op_print(args, _):
    if args is EL:
        print()
        return EL
    end = " "
    while args is not EL:
        x, args = splitcar(args)
        if args is EL:
            end = "\n"
        print(stringify(x), end=end)
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


@glbl("trap")
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


@glbl("type")
def op_type(args, _):
    ## pylint: disable=too-many-return-statements
    (x,) = unpack(args, 1)
    if x is EL:
        return symbol("()")
    if x is T:
        return symbol("#t")
    if isinstance(x, Symbol):
        return symbol("symbol")
    if isinstance(x, int):
        return symbol("int")
    if isinstance(x, float):
        return symbol("float")
    if isinstance(x, str):
        return symbol("str")
    if isinstance(x, Lambda):
        return symbol("lambda")
    if not isinstance(x, list):
        if callable(x):
            return symbol("primitive")
        return symbol("opaque")
    return symbol("list")


@glbl("while")
def op_while(args, e):
    (x,) = unpack(args, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    while leval(x, e) is not EL:
        pass
    return EL


## }}}
## {{{ ffi ops


def module_ffi(module, attr, args):
    if isinstance(attr, Symbol):
        attr = str(attr)
    elif not isinstance(attr, str):
        raise TypeError(f"expeceted str or symbol, got {attr!r}")
    ret = getattr(module, attr)
    if callable(ret):
        return ret(*args)
    if args:
        raise ValueError(f"{attr} takes no args")
    return ret


@ffi("math")
def ffi_math(args, _):
    import math  ## pylint: disable=import-outside-toplevel

    if not args:
        raise ValueError("at least one arg required")
    return module_ffi(math, args[0], args[1:])


@ffi("random")
def ffi_random(args, _):
    import random  ## pylint: disable=import-outside-toplevel

    if not args:
        raise ValueError("at least one arg required")
    return module_ffi(random, args[0], args[1:])


@ffi("range")
def ffi_range(args, _):
    return list(range(*args))


@ffi("shuffle")
def ffi_shuffle(args, _):
    import random  ## pylint: disable=import-outside-toplevel

    (l,) = args
    random.shuffle(l)
    return l


@ffi("time")
def ffi_time(args, _):
    import time  ## pylint: disable=import-outside-toplevel

    if not args:
        raise ValueError("at least one arg required")
    ## time* brokers in time tuples not lists...
    attr = args[0]
    args = [tuple(x) if isinstance(x, list) else x for x in args[1:]]
    return module_ffi(time, attr, args)


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


def main():
    def callback(sexpr):
        try:
            value = leval(sexpr)
        except SystemExit:
            raise
        except:
            print("Offender (pyth):", sexpr)
            print("Offender (lisp):", stringify(sexpr), "\n")
            raise
        if value is not EL:
            print(stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if not stop:
        raise SystemExit(repl(callback))


## }}}
## {{{ stdlib


def boot():
    ## pylint: disable=line-too-long
    parse(
        """
(define list? (lambda (x) (if (eq? (type x) (quote list)) #t ())))
(define pair? list?)
(define list (lambda (& args) args))
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))
(define cadddddr (lambda (l) (car (cdr (cdr (cdr (cdr (cdr l))))))))
(define foreach (lambda (f l)
    (while (lambda ()
        (if
            (null? l)
            ()
            ((lambda (x y z) z)  ;; (do) without (if) since args eval l->r
                (f (car l))
                (set! l (cdr l))
                #t
            )
        )
    ))
))
(define do (lambda (& args) (last args)))
(define reverse (lambda (l) (do
    (define r ())
    (while (lambda ()
        (if
            (null? l)
            ()
            (do
                (set! r (cons (car l) r))
                (set! l (cdr l))
                #t
            )
        )
    ))
    r
)))
(special and (lambda (__special_and2_x__ __special_and2_y__)
    (if (eval __special_and2_x__ 1) (eval __special_and2_y__ 1) ())
))
(special or (lambda (__special_or2_x__ __special_or2_y__)
    (if (eval __special_or2_x__ 1) #t (eval __special_or2_y__ 1))
))
(define join (lambda (x y)
    (if (null? x) y (cons (car x) (join (cdr x) y)))
))
;;; definition of let sicp p.87
(special let (lambda (__special_let_vars__ __special_let_body__) (do
    ;; (let ((x a) (y b)) body)

    (define __special_let_vdecls__ ())  ;; (x y)
    (define __special_let_vvals__ ())   ;; ((eval a) (eval b))
    ;; declare one var
    (define __special_let_decl1__ (lambda (__special_let_decl1_var__ __special_let_decl1_value__) (do
        (set! __special_let_vdecls__ (join __special_let_vdecls__ (list __special_let_decl1_var__)))
        (set! __special_let_vvals__  (join __special_let_vvals__  (list (eval __special_let_decl1_value__))))
    )))
    ;; declare the next item from vars
    (define __special_let_next__ (lambda () (do
        (__special_let_decl1__ (car (car __special_let_vars__)) (car (cdr (car __special_let_vars__))))
        (set! __special_let_vars__ (cdr __special_let_vars__))
    )))
    ;; declare everthing, then return (doit)
    (define __special_let_decls__ (lambda () (
        cond
            ((null? __special_let_vars__)   (__special_let_doit__))
            (#t (do
                    (__special_let_next__) (__special_let_decls__)
            ))
    )))
    (define __special_let_doit__ (lambda () (do
        (define __special_let_doit_head__ (join (list (quote lambda)) (list __special_let_vdecls__)))
        (define __special_let_doit_mid__  (join __special_let_doit_head__ (list __special_let_body__)))
        (define __special_let_doit_lam__  (join (list __special_let_doit_mid__) __special_let_vvals__))
        (eval __special_let_doit_lam__ 1)
    )))
    (__special_let_decls__)
)))
(define length (lambda (l) (do
    (define n 0)
    (while (lambda ()
        (if
            (null? l)
            ()
            (do
                (set! n (add n 1))
                (set! l (cdr l))
                #t
            )
        )
    ))
    n
)))
(define neg (lambda (x) (sub 0 x)))
(define add (lambda (x y) (sub x (neg y))))
(define mod (lambda (n d) (sub n (mul d (div n d)))))
(define abs (lambda (x) (if (lt? x 0) (neg x) x)))
(define copysign (lambda (x y) (if (lt? y 0) (neg (abs x)) (abs x))))
(define le? (lambda (x y) (or (lt? x y) (equal? x y))))
(define ge? (lambda (x y) (not (lt? x y))))
(define gt? (lambda (x y) (not (le? x y))))
(define bnot (lambda (x) (nand x x)))
(define band (lambda (x y) (bnot (nand x y))))
(define bor  (lambda (x y) (nand (bnot x) (bnot y))))
(define bxor (lambda (x y) (band (nand x y) (bor x y))))

;; signed integer multiplication from subtraction and right shift (division)
(define umul (lambda (x y accum)
    (if
        (while (lambda ()  ;; <= this is where it's not portable
            (if
                (equal? 0 x)
                ()
                ((lambda (& _) #t)
                    (if
                        (equal? (band x 1) 1)
                        (set! accum (add accum y))
                        ()
                    )
                    (set! x (div x 2))
                    (set! y (mul y 2))
                )
            )
        ))
        ()
        accum
    )
))
(define smul (lambda (x y) (do
    (define sign 1)
    (if (lt? x 0) (set! sign (neg sign)) ())
    (if (lt? y 0) (set! sign (neg sign)) ())
    (cond
        ((equal? x 0)       0)
        ((equal? y 0)       0)
        ((equal? (abs y) 1) (copysign x sign))
        ((lt? y x)          (copysign (umul (abs y) (abs x) 0) sign))
        (#t                 (copysign (umul (abs x) (abs y) 0) sign))
    )
)))
    """,
        leval,
    )


boot()


## }}}


if __name__ == "__main__":
    main()


## EOF
