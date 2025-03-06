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

"lcore.py -- lisp core"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback

## {{{ exports

__all__ = (
    "Context",
    "EL",
    "Parser",
    "SENTINEL",
    "Symbol",
    "T",
    "car",
    "cdr",
    "cons",
    "create_continuation",
    "create_environment",
    "create_lambda",
    "eq",
    "error",
    "execute",
    "ffi",
    "glbl",
    "is_atom",
    "k_leval",
    "k_stringify",
    "load",
    "main",
    "parse",
    "repl",
    "set_car",
    "set_cdr",
    "spcl",
    "symcheck",
)

## }}}
## {{{ basics


class error(Exception):
    pass


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

    __repr__ = __str__


def is_atom(x):
    return x.__class__ is Symbol or x is EL or x is T


def eq(x, y):
    return x is y and is_atom(x)


def symcheck(x):
    if x.__class__ is Symbol:
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ symbol table


def create_symbol_table():
    t = {}

    def symbol(s):
        if s not in t:
            t[s] = Symbol(s)
        return t[s]

    return symbol


## }}}
## {{{ pairs


def listcheck(x):
    if x.__class__ is list:
        return x
    raise TypeError(f"expected list, got {x!r}")


def car(x):
    return listcheck(x)[0]


def cdr(x):
    return EL if x is EL else listcheck(x)[1]


def cons(x, y):
    return [x, y]


def set_car(x, y):
    listcheck(x)[0] = y


def set_cdr(x, y):
    listcheck(x)[1] = y


## }}}
## {{{ environment


def create_environment(ctx, params, args, parent):
    t = {SENTINEL: parent}
    v = ctx.symbol("&")
    variadic = False
    try:
        while params is not EL:
            p, params = params
            if p.__class__ is not Symbol:
                raise SyntaxError("expected symbol, got {p!r}")
            if p is v:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("trailing junk after &")
                t[p] = args
                return t
            else:
                t[p], args = args
        if variadic:
            raise SyntaxError("params end with &")
        if args is not EL:
            raise SyntaxError("too many args")
        return t
    except TypeError:
        if args is EL:
            raise SyntaxError("not enough args") from None
        raise SyntaxError("expected list") from None


## }}}
## {{{ decorators and global decl table


__G = {}


def glbl(name):
    def wrap(func):
        __G[name] = func
        func.special = func.ffi = False
        return func

    return wrap


def spcl(name):
    def wrap(func):
        __G[name] = func
        func.special = True
        func.ffi = False
        return func

    return wrap


def ffi(name):
    def wrap(func):
        __G[name] = func
        func.special = False
        func.ffi = True
        return func

    return wrap


def create_global_env(ctx):
    symbol = ctx.symbol
    genv = create_environment(ctx, EL, EL, SENTINEL)
    genv[symbol("#t")] = T
    for k, v in __G.items():
        genv[symbol(k)] = v
    return genv


## }}}
## {{{ context


class Context:
    ## pylint: disable=too-many-instance-attributes

    __slots__ = ("argl", "cont", "env", "exp", "val", "s", "symbol", "g")

    def __init__(self):
        ## registers
        self.argl = self.cont = self.env = self.exp = self.val = EL
        ## stack
        self.s = EL
        ## symbols
        self.symbol = create_symbol_table()
        ## global env
        self.g = create_global_env(self)

    ## top level

    def leval(self, x, env=SENTINEL):
        self.cont = self.land
        self.exp = x
        self.env = self.g if env is SENTINEL else env
        return self.trampoline(k_leval)

    def stringify(self, x):
        self.cont = self.land
        self.exp = x
        return self.trampoline(k_stringify)

    ## stack

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

    ## trampoline

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

    ## unpack

    def unpack1(self):
        try:
            x, a = self.argl
            if a is not EL:
                raise TypeError()
            return x
        except TypeError:
            raise SyntaxError("expected one arg") from None

    def unpack2(self):
        try:
            x, a = self.argl
            y, a = a
            if a is not EL:
                raise TypeError()
            return x, y
        except TypeError:
            raise SyntaxError("expected two args") from None

    def unpack3(self):
        try:
            x, a = self.argl
            y, a = a
            z, a = a
            if a is not EL:
                raise TypeError()
            return x, y, z
        except TypeError:
            raise SyntaxError("expected three args") from None

    ## state mgt

    def restore(self, x):
        self.argl, self.cont, self.env, self.exp, self.val, self.s = x

    def save(self):
        return self.argl, self.cont, self.env, self.exp, self.val, self.s


## }}}
## {{{ continuation


def create_continuation(ctx):
    s = ctx.save()

    def continuation(ctx):
        x = ctx.unpack1()
        ctx.restore(s)
        ctx.val = x
        return ctx.cont

    continuation.special = continuation.ffi = False
    continuation.continuation = True

    return continuation


## }}}
## {{{ lambda


def create_lambda(params, body, env):
    def lcall(ctx):
        parent = ctx.env if lcall.special else env
        ctx.env = create_environment(ctx, params, ctx.argl, parent)
        ctx.exp = body
        return k_leval

    lcall.special = lcall.ffi = False
    lcall.lambda_ = params, body

    return lcall


def k_stringify_lambda(ctx):
    ctx.exp, body = ctx.exp.lambda_
    ctx.push(ctx.cont)
    ctx.push(body)
    ctx.cont = k_stringify_lambda_params
    return k_stringify


def k_stringify_lambda_params(ctx):
    ctx.exp = ctx.pop()
    ctx.push(ctx.val)
    ctx.cont = k_stringify_lambda_body
    return k_stringify


def k_stringify_lambda_body(ctx):
    paramstr = ctx.pop()
    ctx.val = "(lambda " + paramstr + " " + ctx.val + ")"
    return ctx.pop()


## }}}
## {{{ stringify


def k_stringify(ctx):
    x = ctx.exp
    t = x.__class__
    if t is list:
        ctx.push(ctx.cont)
        ctx.push(SENTINEL)
        return k_stringify_setup(ctx, x)
    if t in (Symbol, int, float, str):
        ctx.val = str(x)
    elif callable(x):
        if getattr(x, "lambda_", False):
            return k_stringify_lambda
        if getattr(x, "continuation", False):
            ctx.val = "<continuation>"
        else:
            ctx.val = "<primitive>"
    elif x is EL:
        ctx.val = "()"
    elif x is T:
        ctx.val = "#t"
    else:
        ctx.val = "<opaque>"
    return ctx.cont


def k_stringify_setup(ctx, items):
    try:
        ctx.exp, items = items
    except TypeError:
        raise SyntaxError(f"expected list, got {items!r}") from None
    if items is EL:
        ctx.cont = k_stringify_last
    else:
        ctx.push(items)
        ctx.cont = k_stringify_next
    return k_stringify


def k_stringify_next(ctx):
    items = ctx.pop()
    ctx.push(ctx.val)
    return k_stringify_setup(ctx, items)


def k_stringify_last(ctx):
    items = [ctx.val]
    pop = ctx.pop
    while True:
        x = pop()
        if x is SENTINEL:
            break
        items.insert(0, x)
    ctx.val = "(" + " ".join(items) + ")"
    return pop()


## }}}
## {{{ leval


def k_leval(ctx):
    ## pylint: disable=too-many-branches
    x = ctx.exp
    t = x.__class__
    if t is Symbol:
        e = ctx.env
        while e is not SENTINEL:
            try:
                ctx.val = e[x]
                return ctx.cont
            except KeyError:
                e = e[SENTINEL]
        raise NameError(str(x))

    if t is not list:
        ctx.val = x
        return ctx.cont

    op, args = x
    tt = op.__class__
    if tt is Symbol:
        e = ctx.env
        while e is not SENTINEL:
            try:
                op = e[op]
                break
            except KeyError:
                e = e[SENTINEL]
        else:
            raise NameError(str(op))
        try:
            if op.special:
                ctx.argl = args
                return op
        except AttributeError:
            pass

    ctx.s = [args, [ctx.env, [ctx.cont, ctx.s]]]
    try:
        _ = op.__call__
        ctx.val = op
        return k_leval_proc_done
    except AttributeError:
        pass
    if tt is not list:
        raise SyntaxError(f"expected list or proc, got {op!r}")
    ctx.cont = k_leval_proc_done
    ctx.exp = op
    return k_leval


def k_leval_proc_done(ctx):
    proc = ctx.val
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError("expected callable, got {proc!r}") from None
    ctx.argl, s = ctx.s
    ctx.env, s = s

    if ctx.argl is EL or proc.special:
        ctx.cont, ctx.s = s
        return proc

    s = [ctx.env, [SENTINEL, [proc, s]]]
    ctx.exp, args = ctx.argl
    if args is EL:
        ctx.cont = k_leval_last
    elif args.__class__ is list:
        s = [args, s]
        ctx.cont = k_leval_next
    else:
        raise TypeError(f"expected list, got {args!r}")
    ctx.s = s
    return k_leval


def k_leval_next(ctx):
    args, s = ctx.s
    ctx.env, s = s

    s = [ctx.env, [ctx.val, s]]
    ctx.exp, args = args
    if args is EL:
        ctx.cont = k_leval_last
    elif args.__class__ is list:
        s = [args, s]
        ctx.cont = k_leval_next
    else:
        raise TypeError(f"expected list, got {args!r}")
    ctx.s = s
    return k_leval


def k_leval_last(ctx):
    ctx.env, s = ctx.s
    args = [ctx.val, EL]
    while True:
        x, s = s
        if x is SENTINEL:
            break
        args = [x, args]
    ctx.argl = args
    proc, s = s
    ctx.cont, ctx.s = s
    if proc.ffi:
        ctx.exp = proc
        return k_ffi
    return proc


## }}}
## {{{ list builder


class ListBuilder:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def append(self, x):
        n = [x, EL]
        if self.h is EL:
            self.h = n
        else:
            self.t[1] = n
        self.t = n

    def get(self):
        return self.h


## }}}
## {{{ ffi support


def k_ffi(ctx):
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
        ctx.val = x
        return ctx.cont
    if not x:
        ctx.val = EL
        return ctx.cont

    ctx.push(ctx.cont)
    ctx.push(ListBuilder())
    return k_pv2lv_setup(ctx, list(x))


def k_pv2lv_setup(ctx, args):
    ctx.exp = args.pop(0)
    ctx.push(args)
    ctx.cont = k_pv2lv_next
    return k_py_value_to_lisp_value


def k_pv2lv_next(ctx):
    args = ctx.pop()
    argl = ctx.pop()
    argl.append(ctx.val)
    if not args:
        ctx.val = argl.get()
        return ctx.pop()
    ctx.push(argl)
    return k_pv2lv_setup(ctx, args)


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
            self.stack.append(ListBuilder())
        elif ttype == s.T_RPAR:
            del self.qstack[-1]
            l = self.quote_wrap(self.stack.pop().get())
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
        self.stack[-1].append(self.quote_wrap(x))

    def quote_wrap(self, x):
        while self.qstack and self.qstack[-1].__class__ is Symbol:
            x = [self.qstack.pop(), [x, EL]]
        return x


## }}}
## {{{ high level parsing routines


def parse(ctx, text, callback):
    p = Parser(ctx, callback)
    p.feed(text)
    p.feed(None)


def execute(ctx, text):
    results = []

    def callback(expr):
        results.append(ctx.leval(expr))

    parse(ctx, text, callback)
    return results


def load(ctx, filename, callback=None):
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
            parse(ctx, fp.read(), callback)
        else:
            execute(ctx, fp.read())


## }}}
## {{{ repl and main


def repl(ctx, callback):
    try:
        import readline as _  ## pylint: disable=import-outside-toplevel
    except ImportError:
        pass

    ## pylint: disable=unused-variable
    p, rc, stop = Parser(ctx, callback), 0, False

    def feed(x):
        nonlocal p, rc, stop
        try:
            p.feed(x)
        except SystemExit as exc:
            ctx.clear_stack()
            stop, rc = True, exc.args[0]
        except:  ## pylint: disable=bare-except
            ctx.clear_stack()
            p = Parser(ctx, callback)
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


def main(ctx=None, force_repl=False):
    try:
        sys.set_int_max_str_digits(0)
    except AttributeError:
        pass

    if ctx is None:
        ctx = Context()

    def callback(expr):
        try:
            value = ctx.leval(expr)
        except SystemExit:
            raise
        except:
            print("Offender (pyth):", expr)
            print("Offender (lisp):", ctx.stringify(expr), "\n")
            raise
        if value is not EL:
            print(ctx.stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "-":
            stop = False
            break
        load(ctx, filename, callback=callback)
        stop = True
    try:
        if force_repl or not stop:
            raise SystemExit(repl(ctx, callback))
    finally:
        ## debug code can go here
        assert ctx.s is EL, ctx.s
        pass


## }}}

## EOF
