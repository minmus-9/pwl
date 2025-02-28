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

"ctx.py -- register-based lisp with dedicated per-interpreter context"

## pylint: disable=invalid-name,too-many-lines,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


## {{{ exports

__all__ = (
    "Context",
    "EL",
    "Environment",
    "Lambda",
    "LispError",
    "Parser",
    "Queue",
    "SENTINEL",
    "Scanner",
    "Symbol",
    "T",
    "car",
    "cdr",
    "cons",
    "eq",
    "error",
    "ffi",
    "glbl",
    "is_atom",
    "listcheck",
    "set_car",
    "set_cdr",
    "spcl",
    "symcheck",
)

## }}}
## {{{ basics


class LispError(Exception):
    pass


error = LispError


EL = object()
T = True
SENTINEL = object()


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    def __repr__(self):
        return "'" + self.s


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise SyntaxError(f"expected symbol, got {x!r}")


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


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y


## }}}
## {{{ pairs


def listcheck(x):
    if isinstance(x, list):
        return x
    raise TypeError(f"expected list, got {x!r}")


def car(x):
    return listcheck(x)[0]


def cdr(x):
    return listcheck(x)[1]


def cons(x, y):
    return [x, y]


def set_car(x, y):
    listcheck(x)[0] = y


def set_cdr(x, y):
    listcheck(x)[1] = y


## }}}
## {{{ environment


class Environment:
    __slots__ = ("t", "p")

    def __init__(self, ctx, params, args, parent):
        self.p = parent
        self.t = {}
        self.bind(ctx, params, args)

    def bind(self, ctx, params, args):
        t = self.t
        v = ctx.symbol("&")
        variadic = False
        while params is not EL:
            if not isinstance(params, list):
                raise TypeError(f"expected param list, got {params!r}")
            p, params = params
            if not isinstance(p, Symbol):
                raise TypeError(f"expected param symbol, got {p!r}")
            if p is v:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                t[p] = args
                return
            elif isinstance(args, list):
                t[p], args = args
            elif args is EL:
                raise SyntaxError("not enough args")
            else:  ## args is not a list
                raise TypeError(f"expected arg list, got {args!r}")
        if variadic:
            raise SyntaxError("params ends with '&'")
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, sym):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            try:
                return e.t[sym]
            except KeyError:
                e = e.p
        raise NameError(str(sym))

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        self.t[sym] = value

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            if sym in e.t:
                e.t[sym] = value
                return
            e = e.p
        raise NameError(str(sym))


# }}}
## {{{ decorators


GLOBALS = {"#t": T}


def glbl(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = False
        func.ffi = False
        return func

    return wrap


def spcl(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = True
        func.ffi = False
        return func

    return wrap


def ffi(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = False
        func.ffi = True
        return func

    return wrap


## }}}
## {{{ queue


class Queue:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

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
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    special = ffi = False

    __slots__ = ("c", "s")

    def __init__(self, ctx, cont):
        self.c = cont
        self.s = ctx.save()

    def __call__(self, ctx):
        x = ctx.unpack1()
        ctx.restore(self.s)
        ctx.val = x
        return self.c


## }}}
## {{{ lambda


class Lambda:
    ## pylint: disable=too-few-public-methods

    ffi = False

    __slots__ = ("p", "b", "e", "special")

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = False

    def __call__(self, ctx):
        p = ctx.env if self.special else self.e
        ctx.env = ctx.environment(self.p, ctx.argl, p)
        ctx.exp = self.b
        return k_leval

    ###

    def k_lambda_body_done(self, ctx):
        ## pylint: disable=no-self-use
        bodystr = ctx.val
        paramstr = ctx.pop()
        ctx.val = "(lambda " + paramstr + " " + bodystr + ")"
        return ctx.pop()

    def k_lambda_params_done(self, ctx):
        ctx.cont = self.k_lambda_body_done
        ctx.exp = ctx.pop()
        ctx.push(ctx.val)
        return k_stringify

    def k_stringify(self, ctx):
        ctx.push(ctx.cont)
        ctx.push(self.b)
        ctx.cont = self.k_lambda_params_done
        ctx.exp = self.p
        return k_stringify


## }}}
## {{{ context


class Context:
    ## pylint: disable=too-many-instance-attributes,too-many-public-methods

    __slots__ = (
        "s",
        "argl",
        "cont",
        "env",
        "exp",
        "val",
        "symbol",
        "g",
    )

    def __init__(self):
        ## registers
        self.argl = self.cont = self.env = self.exp = self.val = EL

        ## stack
        self.s = EL

        self.symbol = SymbolTable().symbol

        self.g = self.environment(EL, EL, SENTINEL)
        for k, v in GLOBALS.items():
            self.g.set(self.symbol(k), v)

        self.parse(RUNTIME, self.leval)

    ## {{{ top level entry points

    def leval(self, x, env=SENTINEL):
        self.cont = self.land
        self.env = self.g if env is SENTINEL else env
        self.exp = x
        return self.trampoline(k_leval)

    def lisp_value_to_py_value(self, x):
        self.exp = x
        self.cont = self.land
        return self.trampoline(k_lisp_value_to_py_value)

    def py_value_to_lisp_value(self, x):
        self.cont = self.land
        self.exp = x
        return self.trampoline(k_py_value_to_lisp_value)

    def stringify(self, x):
        self.cont = self.land
        self.exp = x
        return self.trampoline(k_stringify)

    ## }}}
    ## {{{ factories

    def continuation(self, cont):
        return Continuation(self, cont)

    def environment(self, params, args, parent):
        return Environment(self, params, args, parent)

    def parser(self, callback):
        return Parser(self, callback)

    ## }}}
    ## {{{ state mgt

    def save(self):
        return self.s, self.argl, self.cont, self.env, self.exp, self.val

    def restore(self, x):
        self.s, self.argl, self.cont, self.env, self.exp, self.val = x

    ## }}}
    ## {{{ registers

    def go(self, val):
        self.val = val
        return self.cont

    ## }}}
    ## {{{ stack

    def clear_stack(self):
        self.s = EL

    def pop(self):
        ret, self.s = self.s
        return ret

    def pop_ce(self):
        self.env, s = self.s
        self.cont, self.s = s

    def push(self, x):
        self.s = [x, self.s]

    def push_ce(self):
        self.s = [self.env, [self.cont, self.s]]

    def top(self):
        return self.s[0]

    ## }}}
    ## {{{ trampoline

    def trampoline(self, func):
        try:
            while True:
                func = func(self)
        except self.Land:
            return self.val

    def land(self, _):
        raise self.Land()

    class Land(Exception):
        pass

    ## }}}
    ## {{{ unpack

    def unpack(self, n):
        args = self.argl
        ret = []
        for _ in range(n):
            if args is EL:
                raise SyntaxError("not enough args")
            if not isinstance(args, list):
                raise SyntaxError(f"expected list, got {args!r}")
            x, args = args
            ret.append(x)
        if args is not EL:
            raise SyntaxError("too many args")
        return ret

    def unpack1(self):
        args = self.argl
        if args is EL:
            raise SyntaxError("need one arg")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        ret, args = args
        if args is not EL:
            raise SyntaxError("too many args")
        return ret

    def unpack2(self):
        args = self.argl
        if args is EL:
            raise SyntaxError("need two args")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        x, args = args
        if args is EL:
            raise SyntaxError("not enough args")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        y, args = args
        if args is not EL:
            raise SyntaxError("too many args")
        return x, y

    def unpack3(self):
        args = self.argl
        if args is EL:
            raise SyntaxError("need three args")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        x, args = args
        if args is EL:
            raise SyntaxError("not enough args")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        y, args = args
        if args is EL:
            raise SyntaxError("not enough args")
        if not isinstance(args, list):
            raise TypeError(f"expected list, got {args!r}")
        z, args = args
        if args is not EL:
            raise SyntaxError("too many args")
        return x, y, z

    ## }}}
    ## {{{ high level parsing routines

    def parse(self, text, callback):
        p = self.parser(callback)
        p.feed(text)
        p.feed(None)

    def execute(self, text):
        results = []

        def callback(expr):
            results.append(self.leval(expr))

        self.parse(text, callback)
        return results

    def load(self, filename, callback=None):
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
                self.parse(fp.read(), callback)
            else:
                self.execute(fp.read())

    ## }}}
    ## {{{ repl and main

    def repl(self, callback):
        try:
            import readline as _  ## pylint: disable=import-outside-toplevel
        except ImportError:
            pass

        ## pylint: disable=unused-variable
        p, rc, stop = self.parser(callback), 0, False

        def feed(x):
            nonlocal p, rc, stop
            try:
                p.feed(x)
            except SystemExit as exc:
                self.clear_stack()
                stop, rc = True, exc.args[0]
            except:  ## pylint: disable=bare-except
                self.clear_stack()
                p = self.parser(callback)
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

    def main(self, force_repl=False):
        try:
            sys.set_int_max_str_digits(0)
        except AttributeError:
            pass

        def callback(expr):
            try:
                value = self.leval(expr)
            except SystemExit:
                raise
            except:
                print("Offender (pyth):", expr)
                print("Offender (lisp):", self.stringify(expr), "\n")
                raise
            if value is not EL:
                print(self.stringify(value))

        stop = True
        for filename in sys.argv[1:]:
            if filename == "-":
                stop = False
                break
            self.load(filename, callback=callback)
            stop = True
        try:
            if force_repl or not stop:
                raise SystemExit(self.repl(callback))
        finally:
            ## debug code can go here
            pass


## }}}


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
    def __init__(self, ctx, callback):
        self.ctx = ctx
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.qt = {
            "'": ctx.symbol("quote"),
            ",": ctx.symbol("unquote"),
            ",@": ctx.symbol("unquote-splicing"),
            "`": ctx.symbol("quasiquote"),
        }

    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.add(self.ctx.symbol(token))
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
        elif ttype in (s.T_INT, s.T_FLOAT, s.T_STRING):
            self.add(token)
        elif ttype in (s.T_TICK, s.T_COMMA, s.T_COMMA_AT, s.T_BACKTICK):
            self.qstack.append(self.qt[token])
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
## {{{ stringify exp -> str


def k_stringify_setup(ctx, rest):
    ctx.exp, rest = rest
    if rest is EL:
        ctx.cont = k_stringify_last
    else:
        ctx.cont = k_stringify_next
        ctx.push(rest)
    return k_stringify


def k_stringify_next(ctx):
    rest = ctx.pop()
    ctx.push(ctx.val)
    return k_stringify_setup(ctx, rest)


def k_stringify_last(ctx):
    parts = [ctx.val]
    while True:
        x = ctx.pop()
        if x is SENTINEL:
            break
        parts.insert(0, x)
    ctx.val = "(" + " ".join(parts) + ")"
    return ctx.pop()


def k_stringify(ctx):
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = ctx.exp
    if x is EL:
        return ctx.go("()")
    if x is T:
        return ctx.go("#t")
    if isinstance(x, (Symbol, int, float, str)):
        return ctx.go(str(x))
    if isinstance(x, Lambda):
        return x.k_stringify
    if isinstance(x, Continuation):
        return ctx.go("[continuation]")
    if callable(x):
        return ctx.go("[primitive]")
    if not isinstance(x, list):
        return ctx.go("[opaque]")

    ctx.push(ctx.cont)
    ctx.push(SENTINEL)
    return k_stringify_setup(ctx, x)


## }}}
## {{{ leval exp -> val, all stack ops inlined for speed


def k_leval(ctx):
    ## pylint: disable=too-many-branches
    ## to speed up python code you have to eliminate function
    ## calls. we aggressively do that in leval_*() for a big
    ## speed boost. it *is* a little opaque in spots
    x = ctx.exp
    t = type(x)
    if t is Symbol:
        ## inline env.get
        e = ctx.env
        while e is not SENTINEL:
            try:
                ctx.val = e.t[x]
                return ctx.cont
            except KeyError:
                e = e.p
        raise NameError(str(x)) from None

    if t is list:
        op, args = x
        if not (isinstance(args, list) or args is EL):
            raise SyntaxError(f"expected arg list, got {args!r}")
        if isinstance(op, Symbol):
            ## inline env.get
            e = ctx.env
            while e is not SENTINEL:
                try:
                    op = e.t[op]
                    break
                except KeyError:
                    e = e.p
            else:
                raise NameError(str(op)) from None

            if getattr(op, "special", False):
                ctx.argl = args
                return op
    elif t is Lambda:
        op = x
        args = EL
    else:
        ctx.val = x
        return ctx.cont

    ## push_ce(); push(args)
    ctx.s = [args, [ctx.env, [ctx.cont, ctx.s]]]

    ## list or sym lookup
    if callable(op):
        ctx.val = op
        return k_leval_proc_done
    if not isinstance(op, list):
        raise SyntaxError(f"expected list or proc, got {op!r}")
    ctx.cont = k_leval_proc_done
    ctx.exp = op
    return k_leval


def k_leval_proc_done(ctx):
    proc = ctx.val
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    ## pop argl and env
    ctx.argl, s = ctx.s  ## NB argl is EL or a pair
    ctx.env, s = s

    if ctx.argl is EL:
        ## pop r.cont
        ctx.cont, ctx.s = s
        return proc

    if proc.special:
        ## pop r.cont
        ctx.cont, ctx.s = s
        return proc

    ## inline old leval_setup() to avoid function call
    ## push proc, SENTINEL, env
    s = [ctx.env, [SENTINEL, [proc, s]]]
    ctx.exp, args = ctx.argl
    if args is EL:
        ctx.cont = k_leval_last
    elif not isinstance(args, list):
        raise TypeError(f"expected arg list, got {args!r}")
    else:
        s = [args, s]
        ctx.cont = k_leval_next
    ctx.s = s
    return k_leval


def k_leval_next(ctx):
    ## pop args and r.env
    args, s = ctx.s  ## NB args is a pair
    ctx.env, s = s
    ## push val and r.env
    s = [ctx.env, [ctx.val, s]]
    ## inline old leval_setup() to avoid function call
    ctx.exp, args = args
    if args is EL:
        ctx.cont = k_leval_last
    elif not isinstance(args, list):
        raise TypeError(f"expected arg list, got {args!r}")
    else:
        ## push args
        s = [args, s]
        ctx.cont = k_leval_next
    ctx.s = s
    return k_leval


def k_leval_last(ctx):
    ## pop r.env
    ctx.env, s = ctx.s
    args = [ctx.val, EL]
    while True:
        ## pop x
        x, s = s
        if x is SENTINEL:
            break
        args = [x, args]
    ctx.argl = args
    ## pop proc
    proc, s = s
    ## pop cont
    ctx.cont, ctx.s = s
    if proc.ffi:
        ctx.exp = proc
        return do_ffi
    return proc


## }}}
## {{{ ffi support


def do_ffi(ctx):
    ctx.push(ctx.cont)
    ctx.push(ctx.exp)  ## proc

    if ctx.argl is EL:
        ctx.argl = []
        return k_ffi_args_done
    ctx.cont = k_ffi_args_done
    ctx.exp = ctx.argl
    return k_lisp_value_to_py_value


def k_ffi_args_done(ctx):
    proc = ctx.pop()
    ctx.cont = ctx.pop()
    ctx.exp = proc(ctx.val)
    return k_py_value_to_lisp_value


def k_lisp_value_to_py_value(ctx):
    x = ctx.exp
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        ctx.val = x
        return ctx.cont
    ctx.push(ctx.cont)
    ctx.push([])
    return k_lv2pv_setup(ctx, x)


def k_lv2pv_setup(ctx, args):
    ctx.exp, args = args
    ctx.push(args)
    ctx.cont = k_lv2pv_next
    return k_lisp_value_to_py_value


def k_lv2pv_next(ctx):
    args = ctx.pop()
    argl = ctx.pop()
    argl.append(ctx.val)
    if args is EL:
        ctx.val = argl
        return ctx.pop()
    ctx.push(argl)
    return k_lv2pv_setup(ctx, args)


def k_py_value_to_lisp_value(ctx):
    x = ctx.exp
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return ctx.go(x)
    if not x:
        return ctx.go(EL)

    ctx.push(ctx.cont)
    ctx.push(Queue())
    return k_pv2lv_setup(ctx, list(x))


def k_pv2lv_setup(ctx, args):
    ctx.exp = args.pop(0)
    ctx.push(args)
    ctx.cont = k_pv2lv_next
    return k_py_value_to_lisp_value


def k_pv2lv_next(ctx):
    args = ctx.pop()
    argl = ctx.pop()
    argl.enqueue(ctx.val)
    if not args:
        ctx.val = argl.head()
        return ctx.pop()
    ctx.push(argl)
    return k_pv2lv_setup(ctx, args)


## }}}
## {{{ special forms


@spcl("cond")
def op_cond(ctx):
    args = ctx.argl
    if args is EL:
        return ctx.go(EL)
    ctx.push(ctx.cont)
    return k_op_cond_setup(ctx, args)


def k_op_cond_setup(ctx, args):
    pc, args = args
    ctx.argl = pc
    ctx.exp, c = ctx.unpack2()
    ctx.push(c)
    ctx.push(ctx.env)
    if args is EL:
        ctx.cont = k_op_cond_last
    else:
        ctx.push(args)
        ctx.cont = k_op_cond_next
    return k_leval


def k_op_cond_next(ctx):
    args = ctx.pop()
    ctx.env = ctx.pop()
    ctx.exp = ctx.pop()
    if ctx.val is not EL:
        ctx.cont = ctx.pop()
        return k_leval
    return k_op_cond_setup(ctx, args)


def k_op_cond_last(ctx):
    ctx.env = ctx.pop()
    ctx.exp = ctx.pop()
    ctx.cont = ctx.pop()
    if ctx.val is EL:
        return ctx.cont
    return k_leval


@spcl("define")
def op_define(ctx):
    sym, value = ctx.unpack2()
    ctx.push(symcheck(sym))
    ctx.push_ce()
    ctx.cont = k_op_define
    ctx.exp = value
    return k_leval


def k_op_define(ctx):
    ctx.pop_ce()
    ctx.env.set(ctx.pop(), ctx.val)
    return ctx.go(EL)


@spcl("if")
def op_if(ctx):
    ctx.exp, c, a = ctx.unpack3()
    ctx.push_ce()
    ctx.push([c, a])
    ctx.cont = k_op_if
    return k_leval


def k_op_if(ctx):
    c, a = ctx.pop()
    ctx.pop_ce()
    ctx.exp = a if ctx.val is EL else c
    return k_leval


@spcl("lambda")
def op_lambda(ctx):
    params, body = ctx.unpack2()
    return ctx.go(Lambda(params, body, ctx.env))


@spcl("quote")
def op_quote(ctx):
    ctx.val = ctx.unpack1()
    return ctx.cont


@spcl("set!")
def op_setbang(ctx):
    sym, value = ctx.unpack2()
    ctx.push(symcheck(sym))
    ctx.push_ce()
    ctx.cont = k_op_setbang
    ctx.exp = value
    return k_leval


def k_op_setbang(ctx):
    ctx.pop_ce()
    sym = ctx.pop()
    ctx.env.setbang(sym, ctx.val)
    return ctx.go(EL)


@spcl("special")
def op_special(ctx):
    sym, value = ctx.unpack2()
    ctx.push(symcheck(sym))
    ctx.push_ce()
    ctx.cont = k_op_special
    ctx.exp = value
    return k_leval


def k_op_special(ctx):
    ctx.pop_ce()
    sym = ctx.pop()
    if not isinstance(ctx.val, Lambda):
        raise TypeError(f"expected lambda, got {ctx.val!r}")
    ctx.val.special = True
    ctx.env.set(sym, ctx.val)
    return ctx.go(EL)


@spcl("trap")
def op_trap(ctx):
    x = ctx.unpack1()
    ok = T
    ctx.push_ce()
    try:
        res = ctx.leval(x, ctx.env)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    ctx.pop_ce()
    return ctx.go([ok, [res, EL]])


## }}}
## {{{ quasiquote


@spcl("quasiquote")
def op_quasiquote(ctx):
    ctx.exp = ctx.unpack1()
    return qq_


def qq_(ctx):
    form = ctx.exp
    if not isinstance(form, list):
        return ctx.go(form)
    app = form[0]
    if eq(app, ctx.symbol("quasiquote")):
        ## XXX proper nesting?
        ctx.argl = form[1]
        return op_quasiquote
    if eq(app, ctx.symbol("unquote")):
        ctx.argl = form
        _, ctx.exp = ctx.unpack2()
        return k_leval
    if eq(app, ctx.symbol("unquote-splicing")):
        _, __ = ctx.unpack2()
        raise LispError("cannot use unquote-splicing here")
    ctx.push_ce()
    ctx.push(SENTINEL)
    return k_qq_setup(ctx, form)


def k_qq_setup(ctx, form):
    elt, form = form
    if not (isinstance(form, list) or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    ctx.push(form)
    ctx.push_ce()
    if isinstance(elt, list) and eq(elt[0], ctx.symbol("unquote-splicing")):
        ctx.argl = elt
        _, ctx.exp = ctx.unpack2()
        ctx.cont = k_qq_spliced
        return k_leval
    ctx.cont = k_qq_next
    ctx.exp = elt
    return qq_


def k_qq_spliced(ctx):
    ctx.pop_ce()
    form = ctx.pop()
    value = ctx.val
    if value is EL:
        if form is EL:
            return k_qq_finish
        return k_qq_setup(ctx, form)
    while value is not EL:
        if not isinstance(value, list):
            raise TypeError(f"expected list, got {value!r}")
        elt, value = value
        if value is EL:
            ctx.val = elt
            ctx.push(form)
            ctx.push_ce()
            return k_qq_next
        ctx.push(elt)
    raise RuntimeError("bugs in the taters")


def k_qq_next(ctx):
    ctx.pop_ce()
    form = ctx.pop()
    ctx.push(ctx.val)
    if form is EL:
        return k_qq_finish
    return k_qq_setup(ctx, form)


def k_qq_finish(ctx):
    ret = EL
    while True:
        x = ctx.pop()
        if x is SENTINEL:
            break
        ret = [x, ret]
    ctx.pop_ce()
    ctx.val = ret
    return ctx.cont


## }}}
## {{{ primitives


def unary(ctx, f):
    x = ctx.unpack1()
    ctx.val = f(x)
    return ctx.cont


def binary(ctx, f):
    x, y = ctx.unpack2()
    ctx.val = f(x, y)
    return ctx.cont


@glbl("apply")
def op_apply(ctx):
    proc, args = ctx.unpack2()
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    ctx.argl = args
    return proc


@glbl("atom?")
def op_atom(ctx):
    def f(x):
        return T if is_atom(x) else EL

    return unary(ctx, f)


@glbl("call/cc")
@glbl("call-with-current-contination")
def op_callcc(ctx):
    x = ctx.unpack1()
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")
    ctx.argl = [ctx.continuation(ctx.cont), EL]
    return x


@glbl("car")
def op_car(ctx):
    def f(x):
        return listcheck(x)[0]

    return unary(ctx, f)


@glbl("cdr")
def op_cdr(ctx):
    def f(x):
        if x is EL:
            return x
        return listcheck(x)[1]

    return unary(ctx, f)


@glbl("cons")
def op_cons(ctx):
    ctx.val = list(ctx.unpack2())
    return ctx.cont


@glbl("div")
def op_div(ctx):
    def f(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(ctx, f)


@glbl("do")
def op_do(ctx):
    x = ctx.argl
    ctx.val = EL
    while x is not EL:
        if not isinstance(x, list):
            raise SyntaxError(f"expected list, got {x!r}")
        ctx.val, x = x
    return ctx.cont


@glbl("eq?")
def op_eq(ctx):
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(ctx, f)


@glbl("equal?")
def op_equal(ctx):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} {y!r}")
        return T if x == y else EL

    return binary(ctx, f)


@glbl("error")
def op_error(ctx):
    x = ctx.unpack1()
    raise LispError(x)


@glbl("eval")
def op_eval(ctx):
    args = ctx.argl
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
        ctx.parse(x, l.append)
        x = l[-1] if l else EL
    e = ctx.env
    for _ in range(n_up):
        e = e.p
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
    ctx.exp = x
    ctx.env = e
    return k_leval


@glbl("exit")
def op_exit(ctx):
    x = ctx.unpack1()
    if isinstance(x, int):
        raise SystemExit(x)
    ctx.exp = x
    ctx.cont = k_op_exit
    return k_stringify


def k_op_exit(ctx):
    raise SystemExit(ctx.val)


@glbl("last")
def op_last(ctx):
    x = ctx.unpack1()
    ret = EL
    while x is not EL:
        ret, x = x
    ctx.val = ret
    return ctx.cont


@glbl("lt?")
def op_lt(ctx):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return T if x < y else EL

    return binary(ctx, f)


@glbl("mul")
def op_mul(ctx):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x * y

    return binary(ctx, f)


@glbl("nand")
def op_nand(ctx):
    def f(x, y):
        if not (isinstance(x, int) and isinstance(y, int)):
            raise TypeError(f"expected integers, got {x!r} and {y!r}")
        return ~(x & y)

    return binary(ctx, f)


@glbl("null?")
def op_null(ctx):
    x = ctx.unpack1()
    ctx.val = T if x is EL else EL
    return ctx.cont


@glbl("print")
def op_print(ctx):
    args = ctx.argl

    if args is EL:
        print()
        ctx.val = EL
        return ctx.cont

    arg, args = args

    ctx.push(ctx.cont)
    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


def k_op_print(ctx):
    args = ctx.pop()

    if args is EL:
        print(ctx.val)
        ctx.val = EL
        return ctx.pop()

    print(ctx.val, end=" ")

    arg, args = args

    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


@glbl("set-car!")
def op_setcarbang(ctx):
    def f(x, y):
        listcheck(x)[0] = y

    return binary(ctx, f)


@glbl("set-cdr!")
def op_setcdrbang(ctx):
    def f(x, y):
        listcheck(x)[1] = y

    return binary(ctx, f)


@glbl("sub")
def op_sub(ctx):
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x - y

    return binary(ctx, f)


@glbl("type")
def op_type(ctx):
    def f(x):
        ## pylint: disable=too-many-return-statements
        if x is EL:
            return ctx.symbol("()")
        if x is T:
            return ctx.symbol("#t")
        if isinstance(x, list):
            return ctx.symbol("pair")
        if isinstance(x, Symbol):
            return ctx.symbol("symbol")
        if isinstance(x, int):
            return ctx.symbol("integer")
        if isinstance(x, float):
            return ctx.symbol("float")
        if isinstance(x, str):
            return ctx.symbol("string")
        if isinstance(x, Lambda):
            return ctx.symbol("lambda")
        if isinstance(x, Continuation):
            return ctx.symbol("continuation")
        if callable(x):
            return ctx.symbol("primitive")
        return ctx.symbol("opaque")

    return unary(ctx, f)


@glbl("while")
def op_while(ctx):
    x = ctx.unpack1()
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    ctx.push(ctx.cont)
    ctx.push(x)
    ctx.push(ctx.env)
    ctx.exp = x
    ctx.cont = k_op_while
    return k_leval


def k_op_while(ctx):
    ctx.env = ctx.pop()
    x = ctx.top()

    if ctx.val is EL:
        ctx.pop()  ## x
        return ctx.pop()
    ctx.push(ctx.env)
    ctx.exp = x
    ctx.cont = k_op_while
    return k_leval


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
        return [tuple(arg) if isinstance(arg, list) else arg for arg in args]

    return module_ffi(f(args), time)


## }}}
## {{{ lisp runtime


RUNTIME = r"""
;; {{{ quasiquote

(special xquasiquote (lambda (x) (qq- x (lambda (x) (eval x 1)))))

(define qq-queue (lambda () (list () ())))

(define qq-hed (lambda (q) (car q)))

(define qq-enq (lambda (q x) (do
    (define n (cons x ()))
    (if
        (null? (car q))
        (set-car! q n)
        (set-cdr! (cdr q) n)
    )
    (set-cdr! q n)
    (car q)
)))

(define qq-lst (lambda (q l) (do
    (if
        (null? l)
        ()
        (do
            (qq-enq q (car l))
            (qq-lst q (cdr l))
        )
    )
    (car q)
)))

(define qq- (lambda (form evaluator) (do
    (if
        (pair? form)
        (qq-pair form evaluator)
        form
    )
)))

(define qq-pair (lambda (form evaluator) (do
    (define q (qq-queue))
    (if
        (null? (cdr (cdr form)))
        (qq-pair-2 form q evaluator)
        (qq-list form q evaluator)
    )
)))

(define qq-pair-2 (lambda (form q evaluator) (do
    (define app (car form))
    (cond
        ((eq? app 'quasiquote) (qq-enq q (qq- (cadr form) evaluator)))  ; XXX correct?
        ((eq? app 'unquote) (evaluator (cadr form)))
        ((eq? app 'unquote-splicing) (error "cannot do unquote-splicing here"))
        (#t (qq-list form q evaluator))
    )
)))

(define qq-list (lambda (form q evaluator) (do
    (if
        (null? form)
        ()
        (do
            (define elt (car form))
            (if
                (pair? elt)
                (if
                    (null? (cdr (cdr elt)))
                    (if
                        (eq? (car elt) 'unquote-splicing)
                        (qq-lst q (evaluator (cadr elt)))
                        (qq-enq q (qq- elt evaluator))
                    )
                    (qq-enq q (qq- elt evaluator))
                )
                (qq-enq q (qq- elt evaluator))
            )
            (qq-list (cdr form) q evaluator)
        )
    )
    (qq-hed q)
)))
;; }}}
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


## }}}


def main():
    Context().main()


if __name__ == "__main__":
    main()


## EOF
