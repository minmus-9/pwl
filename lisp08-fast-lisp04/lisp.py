#!/usr/bin/env python3
##
## pwl04 - python with lisp, version 04-trampolined-fancy
##       https://github.com/minmus-9/pwl04
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
lisp.py -- faster than lisp04-trampolined-fancy
"""


## {{{ header


## pylint: disable=invalid-name,too-many-lines,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


__all__ = (
    "EL",
    "Frame",
    "LispError",
    "Symbol",
    "T",
    "car",
    "cdr",
    "cons",
    "eq",
    "error",
    "execute",
    "ffi",
    "genv",
    "glbl",
    "is_atom",
    "is_pair",
    "leval",
    "load",
    "parse",
    "set_car",
    "set_cdr",
    "spcl",
    "stack",
    "symbol",
    "uncons",
)


## }}}

## {{{ core
## {{{ trampoline


class _Land(Exception):
    pass


def trampoline(func, *args):
    try:
        while True:
            func, args = func(*args)
    except _Land as exc:
        return exc.args[0]


def bounce(func, *args):
    return func, args


def land(value):
    raise _Land(value)


## }}}
## {{{ basics


class LispError(Exception):
    pass


error = LispError


SENTINEL = object()


## }}}
## {{{ atoms


EL = object()
T = True


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    __repr__ = __str__


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y and is_atom(x)


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ symbol table


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("t",)

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        if s not in self.t:
            self.t[s] = Symbol(s)
        return self.t[s]


symbol = SymbolTable().symbol


## }}}
## {{{ pair


def is_pair(x):
    return isinstance(x, list)


def cons(x, y):
    return [x, y]


def car(x):
    return x[0]


def cdr(x):
    return EL if x is EL else x[1]


def set_car(x, y):
    x[0] = y
    return EL


def set_cdr(x, y):
    x[1] = y
    return EL


def uncons(x):
    return x


## }}}
## {{{ queue


class Queue:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

    def head(self):
        return self.h

    def enqueue(self, x):
        node = [x, EL]
        if self.h is EL:
            self.h = node
        else:
            self.t[1] = node
        self.t = node

    def dequeue(self):
        node = self.h
        self.h = node[1]
        if self.h is EL:
            self.t = EL
        return node[0]


## }}}
## {{{ environment and global genv


class Environment:
    __slots__ = ("d", "p")

    def __init__(self, params, args, parent):
        self.p = parent
        self.d = {}
        self.bind(self.d, params, args)

    @staticmethod
    def bind(d, params, args):
        v = symbol("&")
        variadic = False
        while params is not EL:
            p, params = params
            if eq(symcheck(p), v):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                d[p] = args
                return
            elif args is EL:
                raise TypeError("not enough args")
            else:
                d[p] = args[0]
                args = args[1]
        if variadic:
            raise SyntaxError("'&' ends param list")
        if args is not EL:
            raise TypeError("too many args")

    def get(self, sym):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            x = e.d.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        raise NameError(str(sym))

    def set(self, sym, value):
        symcheck(sym)
        self.d[sym] = value
        return EL

    def setbang(self, sym, value):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            if sym in e.d:
                e.d[sym] = value
                return EL
            e = e.p
        raise NameError(str(sym))

    def up(self):  ## for op_eval()
        return self.p


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


## }}}
## {{{ scanner #5: state-based, no lut


class Scanner:
    """
    i am shocked that this krusty coding is the fastest. i have fiddled
    with this class a lot, and this is the fastest implementation i know.
    """

    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_STRING = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_EOF = "eof"

    S_SYM = 0
    S_COMMENT = 1
    S_STRING = 2
    S_ESC = 3
    S_COMMA = 4

    def __init__(self, callback):
        self.pos = [0]  ## yup, a list
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.callback = callback
        self.stab = (  ## this corresponds to the S_* constants
            self.do_sym,
            self.do_comment,
            self.do_string,
            self.do_esc,
            self.do_comma,
        )
        self.state = self.S_SYM

    def feed(self, text):
        if text is None:
            if self.state not in (self.S_SYM, self.S_COMMENT):
                raise SyntaxError("eof in {self.state!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        self.pos[0], n = 0, len(text)
        pos, stab = self.pos, self.stab
        p = 0
        while p < n:
            stab[self.state](text[p])
            p = pos[0] = pos[0] + 1

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
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
        elif ttype != self.T_SYM:
            self.callback(ttype, None)

    def do_sym(self, ch):
        ## pylint: disable=too-many-branches
        if ch in "()[] \n\r\t;\"',`":  ## all of this is actually faster.
            if ch in "([":
                self.parens.append(")" if ch == "(" else "]")
                self.push(self.T_SYM)
                self.push(self.T_LPAR)
            elif ch in ")]":
                if not self.parens:
                    raise SyntaxError(f"too many {ch!r}")
                if self.parens.pop() != ch:
                    raise SyntaxError(f"unexpected {ch!r}")
                self.push(self.T_SYM)
                self.push(self.T_RPAR)
            elif ch in " \n\r\t":
                self.push(self.T_SYM)
            elif ch == ";":
                self.push(self.T_SYM)
                self.state = self.S_COMMENT
            else:
                ## less common cases that aren't delimiters: ["] ['] [,] [`]
                if self.token:
                    raise SyntaxError(f"{ch!r} not a delimiter")
                if ch == '"':
                    self.state = self.S_STRING
                    return
                self.add(ch)
                if ch == "'":
                    self.push(self.T_TICK)
                elif ch == ",":
                    self.state = self.S_COMMA
                else:
                    self.push(self.T_BACKTICK)
        else:
            self.add(ch)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == '"':
            self.push(self.T_STRING)
            self.state = self.S_SYM
        elif ch == "\\":
            self.state = self.S_ESC
        else:
            self.add(ch)

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def do_esc(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STRING

    def do_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos[0] -= 1
            self.push(self.T_COMMA)
        self.state = self.S_SYM


## }}}
## {{{ parser #2: inlined


class Parser:
    QT = {
        "'": symbol("quote"),
        ",": symbol("unquote"),
        ",@": symbol("unquote-splicing"),
        "`": symbol("quasiquote"),
    }

    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.add(symbol(token))
        elif ttype == s.T_LPAR:
            self.qstack.append(")")
            self.stack.append(Queue())
        elif ttype == s.T_RPAR:
            del self.qstack[-1]
            l = self.quote_wrap(self.stack.pop().head())
            if self.stack:
                self.add(l)
            else:
                self.callback(l)
        elif ttype == s.T_INT or ttype == s.T_FLOAT or ttype == s.T_STRING:
            self.add(token)
        elif ttype in (s.T_TICK, s.T_COMMA, s.T_COMMA_AT, s.T_BACKTICK):
            self.qstack.append(self.QT[token])
        else:  ## EOF
            assert not self.stack  ## Scanner checks this
            if self.qstack:
                raise SyntaxError("unclosed quasiquote")

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        while self.qstack and isinstance(self.qstack[-1], Symbol):
            x = [self.qstack.pop(), [x, EL]]
        return x


## }}}
## {{{ high level parsing routines


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
        for d in ["", os.path.dirname(__file__)] + sys.path:
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
        if value is not EL:
            print(stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if force_repl or not stop:
        raise SystemExit(repl(callback))


## }}}
## {{{ stack frame


class Frame:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("x", "c", "e")

    def __init__(self, f, x=None, c=None, e=None):
        self.x = f.x if x is None else x
        self.c = f.c if c is None else c
        self.e = f.e if e is None else e

    def __repr__(self):
        return f"{self.__class__.__name__}({self.x}, {self.c}, {self.e})"


## }}}
## {{{ stack

class Stack:
    __slots__ = ("s",)

    def __init__(self):
        self.s = EL

    def __bool__(self):
        return self.s is not EL

    def clear(self):
        self.s = EL

    def push(self, x):
        self.s = [x, self.s]

    def pop(self):
        ret, self.s = self.s
        return ret

    def top(self):
        return self.s[0]

    ## for continuations

    def get(self):
        return self.s

    def set(self, value):
        self.s = value

    ## avoid inheritance for performance:

    def fpush(self, frame, x=None, c=None, e=None):
        self.s = [Frame(frame, x, c, e), self.s]


## }}}
## }}}

## {{{ global frame stack


stack = Stack()


## }}}
## {{{ lambda


class Lambda:
    __slots__ = ("p", "b", "e", "special")

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = False

    def __call__(self, frame):
        args = frame.x
        p = frame.e if self.special else self.e
        e = Environment(self.p, args, p)
        return bounce(leval_, Frame(frame, x=self.b, e=e))

    ###

    def lambda_body_done(self, bodystr):
        ## pylint: disable=no-self-use
        frame = stack.pop()
        paramstr = frame.x
        return bounce(frame.c, "(lambda " + paramstr + " " + bodystr + ")")

    def lambda_params_done(self, paramstr):
        frame = stack.pop()
        body = frame.x
        stack.fpush(frame, x=paramstr)
        return bounce(
            stringify_, Frame(frame, x=body, c=self.lambda_body_done)
        )

    def stringify_(self, frame):
        stack.fpush(frame, x=self.b)
        return bounce(
            stringify_,
            Frame(frame, x=self.p, c=self.lambda_params_done),
        )


## }}}
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("c", "s")

    def __init__(self, continuation):
        self.c = continuation
        self.s = stack.get()

    def __call__(self, frame):
        (x,) = unpack(frame.x, 1)
        stack.set(self.s)
        return bounce(self.c, x)  ## that's it.


## }}}
## {{{ stringify


def stringify(sexpr, env=SENTINEL):
    e = genv if env is SENTINEL else env
    return trampoline(stringify_, Frame(SENTINEL, x=sexpr, e=e, c=land))


def stringify_setup(frame, args):
    if isinstance(args, list):
        arg, args = args
    else:
        arg = args
        args = EL
        stack.fpush(Frame(frame, x="."))
    stack.fpush(frame, x=args)
    return bounce(stringify_, Frame(frame, x=arg, c=stringify_cont))


def stringify_cont(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        parts = [value]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            parts.insert(0, f.x)
        return bounce(frame.c, "(" + " ".join(parts) + ")")

    stack.fpush(frame, x=value)
    return stringify_setup(frame, args)


def stringify_(frame):
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = frame.x
    if x is EL:
        return bounce(frame.c, "()")
    if x is T:
        return bounce(frame.c, "#t")
    if isinstance(x, (Symbol, int, float, str)):
        return bounce(frame.c, str(x))
    if isinstance(x, Lambda):
        return bounce(x.stringify_, frame)
    if isinstance(x, Continuation):
        return bounce(frame.c, "[continuation]")
    if callable(x):
        return bounce(frame.c, "[primitive]")
    if not isinstance(x, list):
        return bounce(frame.c, "[opaque]")

    stack.fpush(frame, x=SENTINEL)

    return stringify_setup(frame, x)


## }}}
## {{{ eval


def leval(sexpr, env=SENTINEL):
    e = genv if env is SENTINEL else env
    return trampoline(leval_, Frame(SENTINEL, x=sexpr, e=e, c=land))


def eval_setup(frame, args):
    if not isinstance(args, list):
        raise SyntaxError("args must be a proper list")
    arg, args = args
    if args is EL:
        return bounce(leval_, Frame(frame, x=arg, c=eval_last_arg))
    stack.fpush(frame, x=args)
    return bounce(leval_, Frame(frame, x=arg, c=eval_next_arg))


def eval_last_arg(value):
    ret = [value, EL]
    while True:
        f = stack.pop()
        if f.x is SENTINEL:
            break
        ret = [f.x, ret]
    frame = stack.pop()
    proc = frame.x
    ## at this point, need to see if proc is ffi
    if getattr(proc, "ffi", False):
        ## should construct args as a pylist not pair but then Frame would
        ## need a new field to hold proc all the way through. this is about
        ## a global 5% performance hit. i care more about ffi capability
        ## than performance. plus, this thing is slow enough already.
        return bounce(do_ffi, Frame(frame, x=[ret, proc]))
    return bounce(proc, Frame(frame, x=ret))


def eval_next_arg(value):
    frame = stack.pop()
    args = frame.x

    stack.fpush(frame, x=value)
    return eval_setup(frame, args)


def eval_proc_done(proc):
    frame = stack.pop()
    args = frame.x

    if not callable(proc):  ## python func Lambda Continuation
        raise TypeError(f"expected callable, got {proc!r}")

    ## specials don't have their args evaluated
    if getattr(proc, "special", False):
        return bounce(proc, frame)

    ## shortcut the no-args case
    if args is EL:
        ## XXX need to handle ffi here
        return bounce(proc, frame)

    ## evaluate args...

    stack.fpush(frame, x=proc)
    stack.fpush(frame, x=SENTINEL)

    return eval_setup(frame, args)


def leval_(frame):
    ## pylint: disable=too-many-locals

    x = frame.x
    if isinstance(x, Symbol):
        obj = frame.e.get(x)
        return bounce(frame.c, obj)
    if isinstance(x, list):
        op, args = x
    elif isinstance(x, Lambda):
        op = x
        args = EL
    else:
        return bounce(frame.c, x)
    if isinstance(op, Symbol):
        op = frame.e.get(op)
        if getattr(op, "special", False):
            return bounce(op, Frame(frame, x=args))
    if callable(op):
        ## primitive Lambda Continuation
        stack.fpush(frame, x=args)
        return bounce(eval_proc_done, op)
    if not isinstance(op, list):
        raise TypeError(f"expected proc or list, got {op!r}")

    stack.fpush(frame, x=args)
    return bounce(leval_, Frame(frame, x=op, c=eval_proc_done))


## }}}
## {{{ ffi


def do_ffi(frame):
    af = frame.x
    args, func = af
    stack.fpush(frame, x=func)

    if args is EL:
        return bounce(ffi_args_done, [])

    return bounce(
        lisp_value_to_py_value_, Frame(frame, x=args, c=ffi_args_done)
    )


def lisp_value_to_py_value(x):
    return trampoline(lisp_value_to_py_value_, Frame(SENTINEL, x=x, c=land))


def lv2pv_setup(frame, args):
    arg, args = args
    stack.fpush(frame, x=args)
    return bounce(
        lisp_value_to_py_value_, Frame(frame, x=arg, c=lv2pv_next_arg)
    )


def lv2pv_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        ret = [value]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            ret.insert(0, f.x)
        return bounce(frame.c, ret)

    stack.fpush(frame, x=value)
    return lv2pv_setup(frame, args)


def lisp_value_to_py_value_(frame):
    x = frame.x
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        return bounce(frame.c, x)

    stack.fpush(frame, x=SENTINEL)
    return lv2pv_setup(frame, x)


def py_value_to_lisp_value(x):
    return trampoline(py_value_to_lisp_value_, Frame(SENTINEL, x=x, c=land))


def pv2lv_setup(frame, args):
    arg = args.pop(0)
    stack.fpush(frame, x=args)
    return bounce(
        py_value_to_lisp_value_, Frame(frame, x=arg, c=pv2lv_next_arg)
    )


def pv2lv_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if not args:
        ret = [value, EL]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            ret = [f.x, ret]
        return bounce(frame.c, ret)

    stack.fpush(frame, x=value)
    return pv2lv_setup(frame, args)


def py_value_to_lisp_value_(frame):
    x = frame.x
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return bounce(frame.c, x)
    if not x:
        return bounce(frame.c, EL)

    stack.fpush(frame, x=SENTINEL)
    return pv2lv_setup(frame, list(x))


def ffi_args_done(args):
    frame = stack.pop()
    func = frame.x

    ret = func(args)

    return bounce(py_value_to_lisp_value_, Frame(frame, x=ret))


def lisp_list_to_py_list(lst):
    ret = []
    while lst is not EL:
        x, lst = lst
        ret.append(x)
    return ret


def py_list_to_lisp_list(lst):
    q = Queue()
    for x in lst:
        q.enqueue(x)
    return q.head()


## }}}
## {{{ primitive definition decorators


def glbl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.special = True
        return func

    return wrap


def ffi(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.ffi = True
        return func

    return wrap


## }}}
## {{{ unpack


def unpack(args, n):
    ret = []
    for _ in range(n):
        if args is EL:
            raise TypeError(f"not enough args, need {n}")
        ret.append(args[0])
        args = args[1]
    if args is not EL:
        raise TypeError(f"too many args, need {n}")
    return ret


## }}}
## {{{ special forms


def op_cond_setup(frame, args):
    head, args = args
    predicate, consequent = unpack(head, 2)

    stack.fpush(frame, x=[args, consequent])
    return bounce(leval_, Frame(frame, c=op_cond_cont, x=predicate))


def op_cond_cont(value):
    frame = stack.pop()
    args, consequent = frame.x

    if value is not EL:
        return bounce(leval_, Frame(frame, x=consequent))
    if args is EL:
        return bounce(frame.c, EL)
    return op_cond_setup(frame, args)


@spcl("cond")
def op_cond(frame):
    args = frame.x
    if args is EL:
        return bounce(frame.c, EL)

    return op_cond_setup(frame, args)


def op_define_cont(value):
    frame = stack.pop()
    sym = frame.x
    frame.e.set(sym, value)
    return bounce(frame.c, EL)


@spcl("define")
def op_define(frame):
    sym, defn = unpack(frame.x, 2)
    stack.fpush(frame, x=symcheck(sym))
    return bounce(leval_, Frame(frame, x=defn, c=op_define_cont))


###


def op_if_cont(value):
    frame = stack.pop()
    ca = frame.x
    sexpr = ca[1] if value is EL else ca[0]
    return bounce(leval_, Frame(frame, x=sexpr))


@spcl("if")
def op_if(frame):
    p, c, a = unpack(frame.x, 3)
    stack.fpush(frame, x=[c, a])
    return bounce(leval_, Frame(frame, x=p, c=op_if_cont))


###


@spcl("lambda")
def op_lambda(frame):
    params, body = unpack(frame.x, 2)

    if not (isinstance(params, list) or params is EL):
        raise TypeError("expected param list, got {params!r}")

    return bounce(frame.c, Lambda(params, body, frame.e))


@spcl("quote")
def op_quote(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, x)


###


def op_setbang_cont(defn):
    frame = stack.pop()
    sym = frame.x
    frame.e.setbang(sym, defn)
    return bounce(frame.c, EL)


@spcl("set!")
def op_setbang(frame):
    sym, defn = unpack(frame.x, 2)
    stack.fpush(frame, x=symcheck(sym))
    return bounce(leval_, Frame(frame, x=defn, c=op_setbang_cont))


###


def op_special_cont(value):
    frame = stack.pop()
    sym = frame.x
    if not isinstance(value, Lambda):
        raise TypeError(f"expected lambda, got {value!r}")
    value.special = True
    frame.e.set(sym, value)
    return bounce(frame.c, EL)


@spcl("special")
def op_special(frame):
    sym, defn = unpack(frame.x, 2)

    stack.fpush(frame, x=symcheck(sym))
    return bounce(leval_, Frame(frame, x=defn, c=op_special_cont))


###


@spcl("trap")
def op_trap(frame):
    (x,) = unpack(frame.x, 1)
    ok = T
    try:
        ## this has to be recursive because you can't pass
        ## exceptions across the trampoline. there is a chance
        ## of blowing the python stack here if you do a deeply
        ## recursive trap.
        res = leval(x, frame.e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return bounce(frame.c, cons(ok, cons(res, EL)))


## }}}
## {{{ quasiquote


def qq_list_setup(frame, form):
    elt, form = form
    if not (isinstance(form, list) or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    stack.fpush(frame, x=form)
    if isinstance(elt, list) and eq(elt[0], symbol("unquote-splicing")):
        _, x = unpack(elt, 2)
        return bounce(leval_, Frame(frame, x=x, c=qq_spliced))
    return bounce(qq, Frame(frame, x=elt, c=qq_list_cont))


def qq_finish(frame, value):
    res = EL if value is SENTINEL else [value, EL]
    while True:
        f = stack.pop()
        if f.x is SENTINEL:
            break
        res = [f.x, res]
    return bounce(frame.c, res)


def qq_list_cont(value):
    frame = stack.pop()
    form = frame.x

    if form is EL:
        return bounce(qq_finish, frame, value)

    stack.fpush(frame, x=value)

    return qq_list_setup(frame, form)


def qq_spliced(value):
    frame = stack.pop()
    form = frame.x

    if value is EL:
        if form is EL:
            return bounce(qq_finish, frame, SENTINEL)
        return qq_list_setup(frame, form)

    while value is not EL:
        elt, value = value
        if value is EL:
            stack.fpush(frame, x=form)
            return bounce(qq_list_cont, elt)
        stack.fpush(frame, x=elt)

    raise RuntimeError("logs in the bedpan")


def qq_list(frame):
    form = frame.x
    app = form[0]

    if eq(app, symbol("quasiquote")):
        _, x = unpack(form, 2)
        return bounce(qq, Frame(frame, x=x))

    if eq(app, symbol("unquote")):
        _, x = unpack(form, 2)
        return bounce(leval_, Frame(frame, x=x))

    if eq(app, symbol("unquote-splicing")):
        _, x = unpack(form, 2)
        raise LispError("cannot use unquote-splicing here")

    stack.fpush(frame, x=SENTINEL)

    return qq_list_setup(frame, form)


def qq(frame):
    form = frame.x
    if isinstance(form, list):
        return bounce(qq_list, frame)
    return bounce(frame.c, form)


@spcl("quasiquote")
def op_quasiquote(frame):
    (form,) = unpack(frame.x, 1)
    return bounce(qq, Frame(frame, x=form))


## }}}
## {{{ other primitives


def unary(frame, func):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, func(x))


def binary(frame, func):
    x, y = unpack(frame.x, 2)
    return bounce(frame.c, func(x, y))


@glbl(">string")
def op_to_string(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(stringify_, Frame(frame, x=x))


@glbl("apply")
def op_apply(frame):
    proc, args = unpack(frame.x, 2)
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    return bounce(proc, Frame(frame, x=args))


@glbl("atom?")
def op_atom(frame):
    def f(x):
        return T if is_atom(x) else EL

    return unary(frame, f)


@glbl("call/cc")
@glbl("call-with-current-continuation")
def op_callcc(frame):
    (x,) = unpack(frame.x, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")
    cc = Continuation(frame.c)
    arg = [cc, EL]
    return bounce(x, Frame(frame, x=arg))


@glbl("car")
def op_car(frame):
    return unary(frame, car)


@glbl("cdr")
def op_cdr(frame):
    return unary(frame, cdr)


@glbl("cons")
def op_cons(frame):
    return binary(frame, cons)


@glbl("div")
def op_div(frame):
    def f(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(frame, f)


@glbl("do")
def op_do(frame):
    x = frame.x
    ret = EL
    while x is not EL:
        ret, x = x
    return bounce(frame.c, ret)


@glbl("eq?")
def op_eq(frame):
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(frame, f)


@glbl("equal?")
def op_equal(frame):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} {y!r}")

        return T if x == y else EL

    return binary(frame, f)


@glbl("error")
def op_error(frame):
    (x,) = unpack(frame.x, 1)
    raise LispError(x)


@glbl("eval")
def op_eval(frame):

    args = frame.x
    if args is EL:
        raise TypeError("need at least one arg")
    x, args = args
    if args is EL:
        n_up = 0
    else:
        n_up, args = args
        if args is not EL:
            raise TypeError("too many args")

    if isinstance(x, str):
        l = []
        p = Parser(l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    e = frame.e
    for _ in range(n_up):
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
        e = e.up()
    return bounce(leval_, Frame(frame, x=x, e=e))


###


def op_exit_cont(value):
    raise SystemExit(value)


@glbl("exit")
def op_exit(frame):
    (x,) = unpack(frame.x, 1)
    if isinstance(x, int):
        raise SystemExit(x)
    return bounce(stringify_, Frame(frame, x=x, c=op_exit_cont))


###


@glbl("last")
def op_last(frame):
    (x,) = unpack(frame.x, 1)
    ret = EL
    while x is not EL:
        ret, x = x
    return bounce(frame.c, ret)


@glbl("lt?")
def op_lt(frame):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return T if x < y else EL

    return binary(frame, f)


@glbl("mul")
def op_mul2(frame):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x * y

    return binary(frame, f)


@glbl("nand")
def op_nand(frame):
    def f(x, y):
        if not (isinstance(x, int) and isinstance(y, int)):
            raise TypeError(f"expected integers, got {x!r} and {y!r}")
        return ~(x & y)

    return binary(frame, f)


@glbl("null?")
def op_null(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, T if x is EL else EL)


###


def op_print_cont(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        print(value)
        return bounce(frame.c, EL)
    print(value, end=" ")

    arg, args = args

    stack.fpush(frame, x=args)
    return bounce(stringify_, Frame(frame, x=arg, c=op_print_cont))


@glbl("print")
def op_print(frame):
    args = frame.x

    ## NB we know args is a well-formed list because eval() created it

    if args is EL:
        print()
        return bounce(frame.c, EL)

    arg, args = args

    stack.fpush(frame, x=args)
    return bounce(stringify_, Frame(frame, x=arg, c=op_print_cont))


###


@glbl("set-car!")
def op_setcarbang(frame):
    def f(x, y):
        return set_car(x, y)

    return binary(frame, f)


@glbl("set-cdr!")
def op_setcdrbang(frame):
    def f(x, y):
        return set_cdr(x, y)

    return binary(frame, f)


@glbl("sub")
def op_sub(frame):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x - y

    return binary(frame, f)


@glbl("type")
def op_type(frame):
    def f(x):
        ## pylint: disable=too-many-return-statements
        if x is EL:
            return symbol("()")
        if x is T:
            return symbol("#t")
        if isinstance(x, list):
            return symbol("pair")
        if isinstance(x, Symbol):
            return symbol("symbol")
        if isinstance(x, int):
            return symbol("integer")
        if isinstance(x, float):
            return symbol("float")
        if isinstance(x, str):
            return symbol("string")
        if isinstance(x, Lambda):
            return symbol("lambda")
        if isinstance(x, Continuation):
            return symbol("continuation")
        if callable(x):
            return symbol("primitive")
        return symbol("opaque")

    return unary(frame, f)


###


def op_while_cont(value):
    frame = stack.top()
    if value is EL:
        stack.pop()
        return bounce(frame.c, EL)
    return bounce(leval_, Frame(frame, c=op_while_cont))


@glbl("while")
def op_while(frame):
    (x,) = unpack(frame.x, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    stack.fpush(frame, x=x)
    return bounce(leval_, Frame(frame, x=x, c=op_while_cont))


## }}}
## {{{ ffi


def module_ffi(args, module):
    if not args:
        raise TypeError("at least one arg required")
    sym = symcheck(args.pop(0))
    func = getattr(module, str(sym), SENTINEL)
    if func is SENTINEL:
        raise ValueError(f"function {sym!r} does not exist")
    return func(*args)


@ffi("math")
def op_ffi_math(args):
    import math  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, math)


@ffi("random")
def op_ffi_random(args):
    import random  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, random)


@ffi("range")
def op_ffi_range(args):
    return list(range(*args))


@ffi("shuffle")
def op_ffi_shuffle(args):
    import random  ## pylint: disable=import-outside-toplevel

    (l,) = args
    random.shuffle(l)
    return l


@ffi("time")
def op_ffi_time(args):
    import time  ## pylint: disable=import-outside-toplevel

    def f(args):
        ret = []
        for arg in args:
            if isinstance(arg, list):
                arg = tuple(arg)
            ret.append(arg)
        return ret

    return module_ffi(f(args), time)


## }}}

## {{{ lisp runtime


RUNTIME = r"""
;; {{{ basics

;; to accompany quasiquote
(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

;; used everywhere
(define pair? (lambda (x) (if (eq? (type x) 'pair) #t ())))
(define list  (lambda (& args) args))

;; ditto
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))

;; }}}
;; {{{ begin

(define begin do)

;; }}}
;; {{{ foreach
;; call f for each element of lst

(define foreach (lambda (f lst) ( do
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? lst)
        ()
        ( do
            (f (car lst))
            (set! lst (cdr lst))
            (c c)
        )
    )
)))

;; }}}
;; {{{ list-builder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time. it's an ad-hoc queue

(define list-builder (lambda () ( do
    (define ht (list () ()))
    (define add (lambda (x) ( do
        (define node (cons x ()))
        (if
            (null? (car ht))
            ( do
                (set-car! ht node)
                (set-cdr! ht node)
            )
            ( do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            )
        )
        dispatch
    )))
    (define dispatch (lambda (op & args)
        (if
            (eq? op 'add)
            (if
                (null? (cdr args))
                (add (car args))
                (error "add takes a single arg")
            )
            (if
                (eq? op 'extend)
                (if
                    (null? (cdr args))
                    ( do
                        (foreach add (car args))
                        dispatch
                    )
                    (error "extend takes a single list arg")
                )
                (if
                    (eq? op 'get)
                    (car ht)
                    (error "unknown command")
                )
            )
        )
    ))
    dispatch
)))

;; }}}
;; {{{ def

(special def (lambda (__special_def_funcargs__ & __special_def_body__)
    (eval (def$ __special_def_funcargs__ __special_def_body__) 1)))

(define def$ (lambda (funcargs body) ( do
    (if
        (pair? funcargs)
        ()
        (error "def needs a func to define!")
    )
    (define f (car funcargs))
    (define a (cdr funcargs))
    `(define ,f (lambda (,@a) (do ,@body)))
)))

;; }}}
;; {{{ bitwise ops

;; bitwise ops from nand
(def (bnot x)   (nand x x))
(def (band x y) (bnot (nand x y)))
(def (bor  x y) (nand (bnot x) (bnot y)))
(def (bxor x y) (band (nand x y) (bor x y)))

;; }}}
;; {{{ arithmetic

(def (neg x) (sub 0 x))
(def (add x y) (sub x (neg y)))

;; oh, and mod
(def (mod n d) (sub n (mul d (div n d))))

;; absolute value
(def (abs x)
    (if
        (lt? x 0)
        (neg x)
        x
    )
)

;; copysign
(def (copysign x y)
    (if
        (lt? y 0)
        (neg (abs x))
        (abs x)
    )
)

;; (signed) shifts
(def (lshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (add x x))
        (#t             (lshift (lshift x (sub n 1)) 1))
    )
)

(def (rshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (div x 2))
        (#t             (rshift (rshift x (sub n 1)) 1))
    )
)

;; }}}
;; {{{ comparison predicates

(def (le? x y) (if (lt? x y) #t (if (equal? x y) #t ())))
(def (ge? x y) (not (lt? x y)))
(def (gt? x y) (lt? y x))

;; }}}
;; {{{ and or not

(special and (lambda (& __special_and_args__)
    ((lambda (c)
        (cond
            ((null? __special_and_args__) ())
            ((null? (cdr __special_and_args__))
                (eval (car __special_and_args__)))
            ((eval (car __special_and_args__)) ( do
                (set! __special_and_args__ (cdr __special_and_args__))
                (c c)
            ))
            (#t ())
        )
    ) (call/cc (lambda (cc) cc)) )
))

(special or (lambda (& __special_or_args__)
    ((lambda (c)
        (cond
            ((null? __special_or_args__) ())
            ((eval (car __special_or_args__)) #t)
            (#t ( do
                (set! __special_or_args__ (cdr __special_or_args__))
                (c c)
            ))
        )
    ) (call/cc (lambda (cc) cc)) )
))

(def (not x) (if (eq? x ()) #t ()))

;; }}}
;; {{{ assert

(special assert (lambda (__special_assert_sexpr__)
    (if
        (eval __special_assert_sexpr__)
        ()
        (error (>string __special_assert_sexpr__))
    )
))

;; }}}
;; {{{ reverse

(def (reverse l)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        r
        ( do
            (set! r (cons (car l) r))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ iter and enumerate

(def (iter lst fin)
    (define item ())
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (car lst))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

(def (enumerate lst fin)
    (define index 0)
    (define item fin)
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (list index (car lst)))
                    (set! index (add index 1))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

;; }}}
;; {{{ length

(def (length l)
    (define n 0)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        n
        ( do
            (set! n (add n 1))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ fold, transpose, map
;; sicp p.158-165 with interface tweaks
(def (fold-left f initial sequence)
    (define r initial)
    (foreach (lambda (elt) (set! r (f elt r))) sequence)
    r
)

(define reduce fold-left)  ;; python nomenclature

(def (fold-right f initial sequence)
      (fold-left f initial (reverse sequence)))

(define accumulate fold-right)  ;; sicp nomenclature

;(fold-left  cons () (list 1 4 9))  ;; (9 4 1)    (cons 9 (cons 4 (cons 1 ())))
;(fold-right cons () (list 1 4 9))  ;; (1 4 9)    (cons 1 (cons 4 (cons 9 ())))

(def (map1 f lst)
    (def (g elt r) (cons (f elt) r))
    (fold-right g () lst)
)

(def (accumulate-n f initial sequences)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? (car sequences))
        (reverse r)
        ( do
            (set! r (cons (accumulate f initial (map1 car sequences)) r))
            (set! sequences (map1 cdr sequences))
            (c c)
        )
    )
)

(def (transpose lists) (accumulate-n cons () lists))

(def (map f & lists)
    (def (g tuple) (apply f tuple))
    (map1 g (transpose lists))
)

;; }}}
;; {{{ join

(def (join x y)
    (cond
        ((null? x) y)
        ((null? y) x)
        ((null? (cdr x)) (cons (car x) y))
        (#t (fold-right cons (fold-right cons () y) x))
    )
)

;; }}}
;; {{{ queue

(def (queue)
    (define h ())
    (define t ())

    (def (dispatch op & args)
        (cond
            ((eq? op (quote enqueue))
                (if
                    (equal? (length args ) 1)
                    ( do
                        (define node (cons (car args) ()))
                        (if
                            (null? h)
                            (set! h node)
                            (set-cdr! t node)
                        )
                        (set! t node)
                        ()
                    )
                    (error "enqueue takes one arg")
                )
            )
            ((eq? op (quote dequeue))
                (if
                    (equal? (length args) 0)
                        (if
                            (null? h)
                            (error "queue is empty")
                            ( let (
                                (ret (car h)))
                                (do
                                    (set! h (cdr h))
                                    (if (null? h) (set! t ()) ())
                                    ret
                                )
                            )
                        )
                    (error "dequeue takes no args")
                )
            )
            ((eq? op (quote empty?)) (eq? h ()))
            ((eq? op (quote enqueue-many))
                (if
                    (and (equal? (length args) 1) (pair? (car args)))
                    ( do
                        (foreach enqueue (car args))
                        dispatch
                    )
                    (error "enqueue-many takes one list arg")
                )
            )
            ((eq? op (quote get-all)) h)
        )
    )
    dispatch
)


;; }}}
;; {{{ let

(special let (lambda (__special_let_vdefs__ __special_let_body__)
    (eval (let$ __special_let_vdefs__ __special_let_body__) 1)))

(def (let$ vdefs body)
    (define vdecls (transpose vdefs))
    (define vars (car vdecls))
    (define vals (cadr vdecls))
    `((lambda (,@vars) ,body) ,@vals)
)

;; }}}
;; {{{ let*

(special let* (lambda (__special_lets_vdefs__ __special_lets_body__)
    (eval (let*$ __special_lets_vdefs__ __special_lets_body__) 1)))

(def (let*$ vdefs body)
    (if
        (null? vdefs)
        body
        ( do
            (define kv (car vdefs))
            (set! vdefs (cdr vdefs))
            (define k (car kv))
            (define v (cadr kv))
          `((lambda (,k) ,(let*$ vdefs body)) ,v)
        )
    )
)

;; }}}
;; {{{ letrec
;; i saw this (define x ()) ... (set! x value) on stackoverflow somewhere

(special letrec (lambda (__special_letrec_decls__ __special_letrec_body__)
    (eval (letrec$ __special_letrec_decls__ __special_letrec_body__) 1)))

(def (letrec$ decls & body)
    (define names (map1 car decls))
    (define values (map1 cadr decls))
    (def (declare var) `(define ,var ()))
    (def (initialize var-value) `(set! ,(car var-value) ,(cadr var-value)))
    (def (declare-all) (map1 declare names))
    (def (initialize-all) (map1 initialize decls))
    `((lambda () ( do ,@(declare-all) ,@(initialize-all) ,@body)))
)

;; }}}
;; {{{ associative table

(def (table compare)
    (define items ())
    (def (dispatch m & args)
        (cond
            ((eq? m 'known) (not (null? (table$find items key compare))))
            ((eq? m 'del) (set! items (table$delete items (car args) compare)))
            ((eq? m 'get) ( do
                (let* (
                    (key (car args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ()
                        (cadr node)
                    )
                )
            ))
            ((eq? m 'iter) ( do
                (let ((lst items))
                    (lambda ()
                        (if
                            (null? lst)
                            ()
                            ( do
                                (define ret (car lst))
                                (set! lst (cdr lst))
                                ret
                            )
                        )
                    )
                )
            ))
            ((eq? m 'len) (length items))
            ((eq? m 'raw) items)
            ((eq? m 'set) ( do
                (let* (
                    (key (car args))
                    (value (cadr args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ( do
                            (let* (
                                (node (cons key (cons value ()))))
                                (set! items (cons node items)))
                        )
                        (set-car! (cdr node) value)
                    )
                )
            ))
            (#t (error "unknown method"))
        )
    )
    dispatch
)

(def (table$find items key compare)
    (cond
      ((null? items) ())
      ((compare (car (car items)) key) (car items))
      (#t (table$find (cdr items) key compare))
    )
)

(def (table$delete items key compare)
    (define prev ())
    (def (helper assoc key)
        (cond
            ((null? assoc) items)
            ((compare (car (car assoc)) key) (do
                (cond
                    ((null? prev) (cdr assoc))
                    (#t (do (set-cdr! prev (cdr assoc)) items))
                )
            ))
            (#t ( do
                (set! prev assoc)
                (helper (cdr assoc) key)
            ))
        )
    )
    (helper items key)
)

;; }}}
;; {{{ looping: loop, for

;; call f in a loop forever
(def (loop f)
    (define c (call/cc (lambda (cc) cc)))
    (f)
    (c c)
)

;; call f in a loop forever or until (break) is called
(def (loop-with-break f)
    (define brk ())
    (def (break) (brk ()))
    (def (g)
        (define c (call/cc (lambda (cc) cc)))
        (f break)
        (c c)
    )
    (set! brk (call/cc (lambda (cc) cc)))
    (if
        brk
        (g)
        ()
    )
)

;; call f a given number of times as (f counter)
(def (for f start stop step)
    (if (lt? step 1) (error "step must be positive") ())
    (define i start)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? i stop)
        ( do
            (f i)
            (set! i (add i step))
            (c c)
        )
        ()
    )
)

;; }}}
;; {{{ iterate (compose with itself) a function

(def (iter-func f x0 n)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? n 1)
        x0
        (do
            (set! x0 (f x0))
            (set! n (sub n 1))
            (c c)
        )
    )
)

;; }}}
;; {{{ benchmarking

(def (timeit f n)
    (define t0 (time 'time))
    (for f 0 n 1)
    (define t1 (time 'time))
    (define dt (sub t1 t0))
    (if (lt? dt 1e-7) (set! dt 1e-7) ())
    (if (lt? n 1) (set! n 1) ())
    (list n dt (mul 1e6 (div dt n)) (div n dt))
)

;; }}}
;; {{{ gcd

(def (gcd x y)
    (cond
        ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        (#t ( do
            (define c (call/cc (lambda (cc) cc)))
            (if
                (equal? y 0)
                x
                ( do
                    (define r (mod x y))
                    (set! x y)
                    (set! y r)
                    (c c)
                )
            )
        ))
    )
)

;; }}}
;; {{{ smul

;; signed integer multiplication from subtraction and right shift (division)
(define umul (lambda (x y accum)
    ((lambda (c)
        (if
            (equal? 0 x)
            accum
            ((lambda (& _) (c c))
                (if
                    (equal? (band x 1) 1)
                    (set! accum (add accum y))
                    ()
                )
                (set! x (div x 2))
                (set! y (mul y 2))
            )
        )
    ) (call/cc (lambda (cc) cc)))
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

;; }}}

;; EOF
"""


parse(RUNTIME, leval)


## }}}


if __name__ == "__main__":
    main()


## EOF
