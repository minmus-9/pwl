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
lisp.py -- this is the reference for fully trampolined lisp with FFI and
quasiquote. it builds on lisp03/lisp.py
"""

## pylint: disable=invalid-name,too-many-lines,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


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
    ...


SENTINEL = object()


## }}}
## {{{ atoms


class EL_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "()"


EL = EL_()
del EL_


class T_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "#t"


T = T_()
del T_


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["s"]

    def __init__(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        self.s = s

    def __str__(self):
        return self.s

    __repr__ = __str__


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["t"]

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        return self.t.setdefault(s, Symbol(s))


symbol = SymbolTable().symbol


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y and is_atom(x)


## }}}
## {{{ pair


class Pair:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["x", "y"]

    def __init__(self, x, y):
        self.x = x
        self.y = y


def cons(x, y):
    return Pair(x, y)


def car(x):
    if not isinstance(x, Pair):
        raise TypeError(f"expected pair, got {x!r}")
    return x.x


def cdr(x):
    if isinstance(x, Pair):
        return x.y
    if x is EL:
        return EL
    raise TypeError(f"expected () or pair, got {x!r}")


def set_car(x, y):
    if not isinstance(x, Pair):
        raise TypeError(f"expected () or pair, got {x!r}")
    x.x = y
    return EL


def set_cdr(x, y):
    if not isinstance(x, Pair):
        raise TypeError(f"expected () or pair, got {x!r}")
    x.y = y
    return EL


def splitcar(x):
    if not isinstance(x, Pair):
        raise TypeError(f"expected pair, got {x!r}")
    return x.x, x.y


## }}}
## {{{ stack


class Stack:
    __slots__ = ["s"]

    def __init__(self):
        self.s = EL

    def __bool__(self):
        return self.s is EL

    def clear(self):
        self.s = EL

    def push(self, thing):
        self.s = cons(thing, self.s)

    def pop(self):
        ret, self.s = splitcar(self.s)
        return ret

    ## for continuations

    def get(self):
        return self.s

    def set(self, value):
        self.s = value


## }}}
## {{{ stack frame


class Frame:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["x", "c", "e"]

    def __init__(self, f, x=None, c=None, e=None):
        self.x = f.x if x is None else x
        self.c = f.c if c is None else c
        self.e = f.e if e is None else e

    def __repr__(self):
        return f"{self.__class__.__name__}({self.x}, {self.c}, {self.e})"


## }}}
## {{{ frame stack and global stack


class FrameStack(Stack):
    def push(self, thing, **kw):
        super().push(Frame(thing, **kw))


stack = FrameStack()


## }}}
## {{{ queue


class Queue:
    __slots__ = ["h", "t"]

    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

    def head(self):
        return self.h

    def enqueue(self, x):
        node = cons(x, EL)
        if self.h is EL:
            self.h = node
        else:
            set_cdr(self.t, node)
        self.t = node

    def dequeue(self):
        node = self.h
        if node is EL:
            raise ValueError("queue is empty")
        h = self.h = cdr(node)
        if h is EL:
            self.t = EL
        return car(node)


## }}}
## {{{ environment and global genv


class Environment:
    __slots__ = ["p", "d"]

    def __init__(self, params, args, parent):
        self.p = parent
        self.d = {}
        self.bind(params, args)

    def bind(self, params, args):
        pl, al = params, args
        variadic = False
        while params is not EL:
            p, params = splitcar(params)
            if not isinstance(p, Symbol):
                raise TypeError(
                    f"expected symbol, got {p!r} at {pl!r} <= {al!r}"
                )
            if eq(p, symbol("&")):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError(f"extra junk {params!r} after '&'")
                self.d[p] = args
                return
            elif args is EL:
                raise TypeError(f"not enough args at {pl!r} <= {al!r}")
            else:
                a, args = splitcar(args)
                self.d[p] = a
        if variadic:
            raise SyntaxError(f"'&' ends param list {pl!r} <= {al!r}")
        if args is not EL:
            raise TypeError(f"too many args at {pl!r} <= {al!r}")

    def get(self, sym, default):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            x = e.d.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        return default

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        self.d[sym] = value
        return EL

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            if sym in e.d:
                e.d[sym] = value
                return EL
            e = e.p
        raise NameError(str(sym))


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


## }}}
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
        if self.token:
            raise SyntaxError("backtick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_BACKTICK)

    def c_comma(self):
        if self.token:
            raise SyntaxError("comma is not a delimiter")
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
        if self.token:
            raise SyntaxError("quote is not a delimiter")
        self.state = self.S_STR

    def c_rbrack(self):
        if not self.stack:
            raise SyntaxError("too many ']'")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != "]":
            raise SyntaxError(f"expected {c!r}, got ']'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_rpar(self):
        if not self.stack:
            raise SyntaxError("too many ')'")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != ")":
            raise SyntaxError(f"expected {c!r}, got ')'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_tick(self):
        if self.token:
            raise SyntaxError("tick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_TICK)

    def c_ws(self):
        self.push(self.T_SYM)

    def s_bs(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError("bad escape {ch!r}")
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
        if self.stack:
            raise SyntaxError(f"eof in {self.stack[-1]!r}")
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
        l = self.token
        if l:
            t = "".join(l)
            l.clear()
        elif ttype == self.T_SYM:
            return
        else:
            t = None
        if ttype == self.T_SYM:
            if t[0] not in "0123456789.-+":
                return self.callback(ttype, t)
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
            if not self.stack:
                raise SyntaxError("too many ')'s")
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
        elif ttype == self.scanner.T_EOF:
            if self.stack:
                raise SyntaxError("premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(x)

    def filter(self, sexpr):
        ## pylint: disable=no-self-use
        "process ' ` , ,@"
        q = Queue()

        ## NB we know this is a well-formed list
        while sexpr is not EL:
            elt, sexpr = splitcar(sexpr)
            if isinstance(elt, Symbol) and elt in self.q_map:
                elt, sexpr = self.process_syms(elt, sexpr)
            q.enqueue(elt)
        return q.head()

    def process_syms(self, elt, sexpr):
        replacement = self.q_map[elt]
        if sexpr is EL:
            raise SyntaxError(f"got {elt!r} at end of list")
        quoted, sexpr = splitcar(sexpr)
        if isinstance(quoted, Symbol) and quoted in self.q_map:
            quoted, sexpr = self.process_syms(quoted, sexpr)
        elt = cons(replacement, cons(quoted, EL))
        return elt, sexpr


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
## {{{ lambda


class Lambda:
    __slots__ = ["p", "b", "e", "special"]

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
        stack.push(frame, x=paramstr)
        return bounce(
            stringify_, Frame(frame, x=body, c=self.lambda_body_done)
        )

    def stringify_(self, frame):
        stack.push(frame, x=self.b)
        return bounce(
            stringify_,
            Frame(frame, x=self.p, c=self.lambda_params_done),
        )


## }}}
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["continuation", "stack"]

    def __init__(self, continuation):
        self.continuation = continuation  ## a python func
        self.stack = stack.get()

    def __call__(self, frame):
        (x,) = unpack(frame.x, 1)
        stack.set(self.stack)
        return bounce(self.continuation, x)  ## that's it.


## }}}
## {{{ stringify


def stringify(sexpr, env=SENTINEL):
    e = genv if env is SENTINEL else env
    return trampoline(stringify_, Frame(SENTINEL, x=sexpr, e=e, c=land))


def stringify_setup(frame, args):
    if isinstance(args, Pair):
        arg, args = splitcar(args)
    else:
        arg, args = args, EL
        stack.push(Frame(frame, x="."))
    stack.push(frame, x=args)
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

    stack.push(frame, x=value)
    return stringify_setup(frame, args)


def stringify_(frame):
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = frame.x
    if x is EL:
        return bounce(frame.c, "()")
    if x is T:
        return bounce(frame.c, "#t")
    if isinstance(x, (Symbol, int, float)):
        return bounce(frame.c, str(x))
    if isinstance(x, str):
        return bounce(frame.c, '"' + repr(x)[1:-1].replace('"', '\\"') + '"')
    if isinstance(x, Lambda):
        return bounce(x.stringify_, frame)
    if isinstance(x, Continuation):
        return bounce(frame.c, "[continuation]")
    if callable(x):
        return bounce(frame.c, "[primitive]")
    if not isinstance(x, Pair):
        return bounce(frame.c, "[opaque]")

    stack.push(frame, x=SENTINEL)

    return stringify_setup(frame, x)


## }}}
## {{{ eval


def leval(sexpr, env=SENTINEL):
    e = genv if env is SENTINEL else env
    return trampoline(leval_, Frame(SENTINEL, x=sexpr, e=e, c=land))


def eval_setup(frame, args):
    if isinstance(args, Pair):
        arg, args = splitcar(args)
    else:
        arg, args = args, EL
    stack.push(frame, x=args)
    return bounce(leval_, Frame(frame, x=arg, c=eval_next_arg))


def eval_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        ret = cons(value, EL)
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                proc = f.c  ## NB abuse of .c field
                break
            ret = cons(f.x, ret)
        ## at this point, need to see if proc is ffi
        if getattr(proc, "ffi", False):
            ## XXX construct args as a list not pair
            return bounce(do_ffi, Frame(frame, x=cons(ret, proc)))
        return bounce(proc, Frame(frame, x=ret))

    stack.push(frame, x=value)
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
        return bounce(proc, frame)

    ## evaluate args...

    stack.push(frame, c=proc, x=SENTINEL)  ## NB abuse .c field

    return eval_setup(frame, args)


def leval_(frame):
    ## pylint: disable=too-many-locals

    x = frame.x
    if isinstance(x, Symbol):
        obj = frame.e.get(x, SENTINEL)
        if obj is SENTINEL:
            raise NameError(x)
        return bounce(frame.c, obj)
    if isinstance(x, Pair):
        sym, args = splitcar(x)
    elif isinstance(x, Lambda):
        sym, args = x, EL
    else:
        return bounce(frame.c, x)
    if isinstance(sym, Symbol):
        op = frame.e.get(sym, SENTINEL)
        if op is not SENTINEL and getattr(op, "special", False):
            return bounce(op, Frame(frame, x=args))
    elif callable(sym):
        ## primitive Lambda Continuation
        stack.push(frame, x=args)
        return bounce(eval_proc_done, sym)
    elif not isinstance(sym, Pair):
        raise TypeError(f"expected proc or list, got {sym!r}")

    stack.push(frame, x=args)
    return bounce(leval_, Frame(frame, x=sym, c=eval_proc_done))


## }}}
## {{{ ffi


def do_ffi(frame):
    af = frame.x
    args, func = splitcar(af)
    stack.push(frame, x=func)

    if args is EL:
        return bounce(ffi_args_done, [])

    return bounce(
        lisp_value_to_py_value_, Frame(frame, x=args, c=ffi_args_done)
    )


def lisp_value_to_py_value(x):
    return trampoline(lisp_value_to_py_value_, Frame(SENTINEL, x=x, c=land))


def lv2pv_setup(frame, args):
    arg, args = splitcar(args)
    stack.push(frame, x=args)
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

    stack.push(frame, x=value)
    return lv2pv_setup(frame, args)


def lisp_value_to_py_value_(frame):
    x = frame.x
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, Pair):
        return bounce(frame.c, x)

    stack.push(frame, x=SENTINEL)
    return lv2pv_setup(frame, x)


def py_value_to_lisp_value(x):
    return trampoline(py_value_to_lisp_value_, Frame(SENTINEL, x=x, c=land))


def pv2lv_setup(frame, args):
    arg, args = args[0], args[1:]
    stack.push(frame, x=args)
    return bounce(
        py_value_to_lisp_value_, Frame(frame, x=arg, c=pv2lv_next_arg)
    )


def pv2lv_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if not args:
        ret = cons(value, EL)
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            ret = cons(f.x, ret)
        return bounce(frame.c, ret)

    stack.push(frame, x=value)
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

    stack.push(frame, x=SENTINEL)
    return pv2lv_setup(frame, list(x))


def ffi_args_done(args):
    frame = stack.pop()
    func = frame.x

    ret = func(args)

    return bounce(py_value_to_lisp_value_, Frame(frame, x=ret))


def lisp_list_to_py_list(lst):
    ret = []
    while lst is not EL:
        x, lst = splitcar(lst)
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
        arg, args = splitcar(args)
        ret.append(arg)
    if args is not EL:
        raise TypeError(f"too many args, need {n}")
    return ret


## }}}
## {{{ special forms


def op_cond_setup(frame, args):
    head, args = splitcar(args)
    predicate, consequent = unpack(head, 2)

    stack.push(frame, x=cons(args, consequent))
    return bounce(leval_, Frame(frame, c=op_cond_cont, x=predicate))


def op_cond_cont(value):
    frame = stack.pop()
    args, consequent = splitcar(frame.x)

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

    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")

    stack.push(frame, x=sym)
    return bounce(leval_, Frame(frame, x=defn, c=op_define_cont))


###


def op_if_cont(value):
    frame = stack.pop()
    ca = frame.x
    sexpr = cdr(ca) if value is EL else car(ca)
    return bounce(leval_, Frame(frame, x=sexpr))


@spcl("if")
def op_if(frame):
    p, c, a = unpack(frame.x, 3)
    stack.push(frame, x=cons(c, a))
    return bounce(leval_, Frame(frame, x=p, c=op_if_cont))


###


@spcl("lambda")
def op_lambda(frame):
    params, body = unpack(frame.x, 2)

    if not (isinstance(params, Pair) or params is EL):
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
    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")
    stack.push(frame, x=sym)
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

    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")

    stack.push(frame, x=sym)
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
    elt, form = splitcar(form)
    if not (isinstance(form, Pair) or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    stack.push(frame, x=form)
    return bounce(qq_list_next, Frame(frame, x=elt, c=qq_list_cont))


def qq_finish(frame, value):
    res = EL if value is SENTINEL else cons(value, EL)
    while True:
        f = stack.pop()
        if f.x is SENTINEL:
            break
        res = cons(f.x, res)
    return bounce(frame.c, res)


def qq_list_cont(value):
    frame = stack.pop()
    form = frame.x

    if form is EL:
        return bounce(qq_finish, frame, value)

    stack.push(frame, x=value)

    return qq_list_setup(frame, form)


def qq_spliced(value):
    frame = stack.pop()
    form = frame.x

    if value is EL:
        if form is EL:
            return bounce(qq_finish, frame, SENTINEL)
        return qq_list_setup(frame, form)

    while value is not EL:
        if not isinstance(value, Pair):
            raise TypeError(f"expected list, got {value!r}")
        elt, value = splitcar(value)
        if value is EL:
            stack.push(frame, x=form)
            return bounce(qq_list_cont, elt)
        stack.push(frame, x=elt)

    raise RuntimeError("logs in the bedpan")


def qq_list_next(frame):
    elt = frame.x

    if isinstance(elt, Pair) and eq(car(elt), symbol("unquote-splicing")):
        _, x = unpack(elt, 2)
        return bounce(leval_, Frame(frame, x=x, c=qq_spliced))
    return bounce(qq, Frame(frame, x=elt, c=qq_list_cont))


def qq_list(frame):
    form = frame.x
    app = car(form)

    if eq(app, symbol("quasiquote")):
        _, x = unpack(form, 2)
        return bounce(qq, Frame(frame, x=x))

    if eq(app, symbol("unquote")):
        _, x = unpack(form, 2)
        return bounce(leval_, Frame(frame, x=x))

    if eq(app, symbol("unquote-splicing")):
        _, x = unpack(form, 2)
        raise LispError("cannot use unquote-splicing here")

    stack.push(frame, x=SENTINEL)

    return qq_list_setup(frame, form)


def qq(frame):
    form = frame.x
    if isinstance(form, Pair):
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
    arg = cons(cc, EL)
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
        ret, x = splitcar(x)
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

    try:
        (x,) = unpack(frame.x, 1)
        n_up = 0
    except TypeError:
        x, n_up = unpack(frame.x, 2)

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
        e = e.p
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
        ret, x = splitcar(x)
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
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
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

    arg, args = splitcar(args)

    stack.push(frame, x=args)
    return bounce(stringify_, Frame(frame, x=arg, c=op_print_cont))


@glbl("print")
def op_print(frame):
    args = frame.x

    ## NB we know args is a well-formed list because eval() created it

    if args is EL:
        print()
        return bounce(frame.c, EL)

    arg, args = splitcar(args)

    stack.push(frame, x=args)
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
        if isinstance(x, Pair):
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
    frame = stack.pop()

    if value is EL:
        return bounce(frame.c, EL)
    stack.push(frame)
    return bounce(leval_, Frame(frame, c=op_while_cont))


@glbl("while")
def op_while(frame):
    (x,) = unpack(frame.x, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    stack.push(frame, x=x)
    return bounce(leval_, Frame(frame, x=x, c=op_while_cont))


## }}}
## {{{ ffi


def module_ffi(args, module):
    if not args:
        raise TypeError("at least one arg required")
    sym = args.pop(0)
    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")
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


if __name__ == "__main__":
    main()


## EOF
