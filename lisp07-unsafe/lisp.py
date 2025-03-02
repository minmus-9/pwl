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


## {{{ scanner


class Scanner:
    ##  pylint: disable=too-many-instance-attributes

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

    def __init__(self, callback):
        self.callback = callback
        self.token = []
        self.add = self.token.append
        self.pos = 0
        self.state = self.S_SYM
        self.stack = ""
        self.c_map = {
            "(": self.c_lpar,
            ")": self.c_rpar,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_cmnt,
            "'": self.c_tick,
            "`": self.c_backtick,
            ",": self.c_comma,
            '"': self.c_quote,
        }
        self.lookup = self.c_map.get
        self.s_map = {
            self.S_BS: self.s_bs,
            self.S_COMMA: self.s_comma,
            self.S_CMNT: self.s_comment,
            self.S_STR: self.s_str,
            self.S_SYM: self.s_sym,
        }

    def c_backtick(self):
        self.push(self.T_SYM)
        self.push(self.T_BACKTICK)

    def c_comma(self):
        self.state = self.S_COMMA

    def c_cmnt(self):
        self.state = self.S_CMNT

    def c_lbrack(self):
        self.stack += "]"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_lpar(self):
        self.stack += ")"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_quote(self):
        self.state = self.S_STR

    def c_rbrack(self):
        c, self.stack = self.stack[-1], self.stack[:-1]
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_rpar(self):
        c, self.stack = self.stack[-1], self.stack[:-1]
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_tick(self):
        self.push(self.T_SYM)
        self.push(self.T_TICK)

    def c_ws(self):
        self.push(self.T_SYM)

    def s_bs(self, ch):
        c = self.ESC.get(ch)
        self.add(c)
        self.state = self.S_STR

    def s_comma(self, ch):
        if ch == "@":
            self.push(self.T_COMMA_AT)
        else:
            self.push(self.T_COMMA)
            self.pos -= 1
        self.state = self.S_SYM

    def s_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def s_str(self, ch):
        if ch == '"':
            self.state = self.S_SYM
            self.push(self.T_STR)
        elif ch == "\\":
            self.state = self.S_BS
        else:
            self.add(ch)

    def s_sym(self, ch):
        if ch in " \n\r\t":
            self.push(self.T_SYM)
        else:
            f = self.lookup(ch)
            if f:
                f()
            else:
                self.add(ch)

    def eof(self):
        self.push(self.T_SYM)
        self.push(self.T_EOF)

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            return self.eof()
        self.pos, n = 0, len(text)
        while self.pos < n:
            ch = text[self.pos]
            self.pos += 1
            self.s_map[self.state](ch)

    def push(self, ttype):
        t = "".join(self.token)
        del self.token[:]
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


## }}}
## {{{ parser


class Parser:
    def __init__(self, callback):
        self.callback = callback
        self.stack = []
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
            self.stack.append(Queue())
        elif ttype == self.scanner.T_RPAR:
            q = self.stack.pop()
            l = self.filter(q.head())
            if not self.stack:
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

    def add(self, x):
        self.stack[-1].enqueue(x)

    def filter(self, sexpr):
        ## pylint: disable=no-self-use
        "process ' ` , ,@"
        q = Queue()

        ## NB we know this is a well-formed list
        while sexpr is not EL:
            elt, sexpr = sexpr
            if isinstance(elt, Symbol) and elt in self.q_map:
                elt, sexpr = self.process_syms(elt, sexpr)
            q.enqueue(elt)
        return q.head()

    def process_syms(self, elt, sexpr):
        replacement = self.q_map[elt]
        quoted, sexpr = sexpr
        if isinstance(quoted, Symbol) and quoted in self.q_map:
            quoted, sexpr = self.process_syms(quoted, sexpr)
        elt = [replacement, [quoted, EL]]
        return elt, sexpr


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
        self.s = s

    def __str__(self):
        return self.s


SYMTAB = {}


def symbol(s):
    if s not in SYMTAB:
        SYMTAB[s] = Symbol(s)
    return SYMTAB[s]


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y


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
            param, params = params
            if eq(param, symbol("&")):
                variadic = True
            elif variadic:
                self.t[param] = args
                return
            else:
                arg, args = args
                self.t[param] = arg

    def get(self, sym, default):
        e = self
        while e is not SENTINEL:
            x = e.t.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        return default

    def set(self, sym, value):
        self.t[sym] = value
        return EL

    def setbang(self, sym, value):
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
        n = [x, EL]
        if self.h is EL:
            self.h = n
        else:
            self.t[1] = n
        self.t = n

    def dequeue(self):
        n = self.h
        self.h = n[1]
        if self.h is EL:
            self.t = EL
        return n[0]

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
        y, x = x
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
        sym, args = x
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
    else:
        proc = leval(sym, e)
        assert proc is not True

    q = Queue()
    while args is not EL:
        arg, args = args
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
        x, args = args
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
        arg, args = args
        ret.append(arg)
    return ret


## }}}
## {{{ special forms


@spcl("cond")
def op_cond(args, e):
    while args is not EL:
        arg, args = args
        predicate, consequent = unpack(arg, 2)
        if leval(predicate, e) is not EL:
            return leval(consequent, e)
    return EL


@spcl("define")
def op_define(args, e):
    name, value = unpack(args, 2)
    return e.set(name, leval(value, e))


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
    e.setbang(name, leval(value, e))
    return EL


@spcl("special")
def op_special(args, e):
    name, value = unpack(args, 2)
    value = leval(value, e)
    value.special = True
    return e.set(name, value)


## }}}
## {{{ quasiquote


def qq_list(form, e):
    q = Queue()
    while form is not EL:
        elt, form = form
        if isinstance(elt, list) and eq(elt[0], symbol("unquote-splicing")):
            _, x = unpack(elt, 2)
            y = leval(x, e)
            if y is not EL:
                listcheck(y)
                while y is not EL:
                    z, y = y
                    q.enqueue(z)
        else:
            q.enqueue(qq(elt, e))
    return q.head()


def qq_pair(form, e):
    h = form[0]
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
    def f(x):
        return x[0]

    return unary(args, f)


@glbl("cdr")
def op_cdr(args, _):
    def f(x):
        return x[1]

    return unary(args, f)


@glbl("cons")
def op_cons(args, _):
    def f(x, y):
        return [x, y]

    return binary(args, f)


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
        ret, x = x
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
        x, args = args
        if args is EL:
            end = "\n"
        print(stringify(x), end=end)
    return EL


@glbl("set-car!")
def op_setcar(args, _):
    def f(x, y):
        x[0] = y

    return binary(args, f)


@glbl("set-cdr!")
def op_setcdr(args, _):
    def f(x, y):
        x[1] = y

    return binary(args, f)


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
    return [ok, [res, EL]]


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

    while leval(x, e) is not EL:
        pass
    return EL


## }}}
## {{{ ffi ops


def module_ffi(module, attr, args):
    ret = getattr(module, str(attr))
    if callable(ret):
        return ret(*args)
    return ret


@ffi("math")
def ffi_math(args, _):
    import math  ## pylint: disable=import-outside-toplevel

    return module_ffi(math, args[0], args[1:])


@ffi("random")
def ffi_random(args, _):
    import random  ## pylint: disable=import-outside-toplevel

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
