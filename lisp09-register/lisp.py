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
reg.py -- register based lisp from chapter 5 of sicp. this is the fastest yet
"""

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring


import locale
import os
import sys
import traceback

## {{{ exports

__all__ = (
    "Continuation",
    "EL",
    "Environment",
    "Lambda",
    "LispError",
    "Parser",
    "Queue",
    "Registers",
    "SENTINEL",
    "Scanner",
    "Stack",
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
    "go",
    "is_atom",
    "land",
    "leval",
    "leval_",
    "listcheck",
    "load",
    "main",
    "parse",
    "pop",
    "pop_ce",
    "push",
    "push_ce",
    "r",
    "repl",
    "set_car",
    "set_cdr",
    "spcl",
    "stack",
    "stringify",
    "stringify_",
    "symbol",
    "symcheck",
    "top",
    "trampoline",
    "unpack1",
    "unpack2",
    "unpack3",
)

## }}}
## {{{ basics


class LispError(Exception):
    pass


error = LispError
SENTINEL = object()
EL = object()
T = object()


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("t",)

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        if s not in self.t:
            self.t[s] = Symbol(s)
        return self.t[s]


symbol = SymbolTable().symbol


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ pairs, not used internally for performance


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
## {{{ registers, stack ops inlined for speed


class Registers:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("argl", "cont", "env", "exp", "val")

    def __init__(self):
        self.argl = self.cont = self.env = self.exp = self.val = EL

    def push_ce(self):
        stack.s = [self.env, [self.cont, stack.s]]

    def pop_ce(self):
        self.env, s = stack.s
        self.cont, stack.s = s

    def go(self, val):
        self.val = val
        return self.cont

    def get(self):
        return self.argl, self.cont, self.env, self.exp, self.val

    def set(self, x):
        self.argl, self.cont, self.env, self.exp, self.val = x


r = Registers()
push_ce = r.push_ce
pop_ce = r.pop_ce
go = r.go

regs_get = r.get
regs_set = r.set


## }}}
## {{{ trampoline


def trampoline(func):
    try:
        while True:
            func = func()
    except Land_:
        return r.val


class Land_(Exception):
    pass


def land():
    raise Land_()


## }}}
## {{{ stack and global stack


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

    def get(self):
        return self.s

    def set(self, x):
        self.s = x


stack = Stack()
push = stack.push
pop = stack.pop
top = stack.top

stack_get = stack.get
stack_set = stack.set


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
## {{{ env and global env


class Environment:
    __slots__ = ("d", "p")

    def __init__(self, params, args, parent):
        self.d = {}
        self.p = parent
        self.bind(self.d, params, args)

    def bind(self, d, params, args):
        ## pylint: disable=no-self-use
        v = symbol("&")
        variadic = False
        while params is not EL:
            p, params = params
            if not isinstance(p, Symbol):
                raise TypeError(f"expected symbol, got {p!r}")
            if p is v:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                d[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                d[p], args = args
        if variadic:
            raise SyntaxError("params ends with '&'")
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, sym):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while True:
            try:
                return e.d[sym]
            except KeyError:
                e = e.p
                if e is SENTINEL:
                    raise NameError(str(sym)) from None

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        self.d[sym] = value

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while True:
            if sym in e.d:
                e.d[sym] = value
                return EL
            e = e.p
            if e is SENTINEL:
                raise NameError(str(sym))

    def up(self):
        return self.p


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


## }}}
## {{{ op decorators


def glbl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.special = func.ffi = False
        return func

    return wrap


def spcl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.special = True
        func.ffi = False
        return func

    return wrap


def ffi(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.ffi = True
        func.special = False
        return func

    return wrap


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
        elif ttype in (s.T_INT, s.T_FLOAT, s.T_STRING):
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
            stack.clear()
            stop, rc = True, exc.args[0]
        except:  ## pylint: disable=bare-except
            stack.clear()
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
    try:
        if force_repl or not stop:
            raise SystemExit(repl(callback))
    finally:
        assert not stack


## }}}
## {{{ unpack argl -> (...)


def unpack1():
    if r.argl is EL:
        raise SyntaxError("not enough args, need 1")
    ret, x = r.argl
    if x is not EL:
        raise SyntaxError("too many args, need 1")
    return ret


def unpack2():
    if r.argl is EL:
        raise SyntaxError("not enough args, need 2")
    x, args = r.argl
    if args is EL:
        raise SyntaxError("not enough args, need 2")
    y, args = args
    if args is not EL:
        raise SyntaxError("too many args, need 2")
    return x, y


def unpack3():
    if r.argl is EL:
        raise SyntaxError("not enough args, need 3")
    x, args = r.argl
    if args is EL:
        raise SyntaxError("not enough args, need 3")
    y, args = args
    if args is EL:
        raise SyntaxError("not enough args, need 3")
    z, args = args
    if args is not EL:
        raise SyntaxError("too many args, need 3")
    return x, y, z


## }}}
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("c", "r", "s")

    ffi = special = False

    def __init__(self, continuation):
        self.c = continuation
        self.r = regs_get()
        self.s = stack_get()

    def __call__(self):
        x = unpack1()
        regs_set(self.r)
        stack_set(self.s)
        r.val = x
        return self.c  ## that's it


## }}}
## {{{ lambda (argl) -> val


class Lambda:
    __slots__ = ("p", "b", "e", "special")

    ffi = False

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = False

    def __call__(self):
        p = r.env if self.special else self.e
        r.env = Environment(self.p, r.argl, p)
        r.exp = self.b
        return leval_

    ###

    def lambda_body_done(self):
        ## pylint: disable=no-self-use
        bodystr = r.val
        paramstr = pop()
        r.val = "(lambda " + paramstr + " " + bodystr + ")"
        return pop()

    def lambda_params_done(self):
        r.cont = self.lambda_body_done
        r.exp = pop()
        push(r.val)
        return stringify_

    def stringify_(self):
        push(r.cont)
        push(self.b)
        r.cont = self.lambda_params_done
        r.exp = self.p
        return stringify_


## }}}
## {{{ stringify exp -> str


def stringify(expr):
    r.exp = expr
    r.cont = land
    return trampoline(stringify_)


def stringify_setup_(rest):
    r.exp, rest = rest
    if rest is EL:
        r.cont = stringify_last_
    else:
        r.cont = stringify_next_
        push(rest)
    return stringify_


def stringify_next_():
    rest = pop()
    push(r.val)
    return stringify_setup_(rest)


def stringify_last_():
    parts = [r.val]
    while True:
        x = pop()
        if x is SENTINEL:
            break
        parts.insert(0, x)
    r.val = "(" + " ".join(parts) + ")"
    return pop()


def stringify_():
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = r.exp
    if x is EL:
        return go("()")
    if x is T:
        return go("#t")
    if isinstance(x, (Symbol, int, float, str)):
        return go(str(x))
    if isinstance(x, Lambda):
        return x.stringify_
    if isinstance(x, Continuation):
        return go("[continuation]")
    if callable(x):
        return go("[primitive]")
    if not isinstance(x, list):
        return go("[opaque]")

    push(r.cont)
    push(SENTINEL)
    return stringify_setup_(x)


## }}}
## {{{ leval exp -> val, all stack ops inlined for speed


def leval(exp, env=SENTINEL):
    r.cont = land
    r.env = genv if env is SENTINEL else env
    r.exp = exp
    return trampoline(leval_)


def leval_():
    ## pylint: disable=too-many-branches
    ## to speed up python code you have to eliminate function
    ## calls. we aggressively do that in leval_*() for a big
    ## speed boost. it *is* a little opaque in spots
    x = r.exp
    t = type(x)
    if t is Symbol:
        ## inline env.get
        e = r.env
        while e is not SENTINEL:
            try:
                r.val = e.d[x]
                return r.cont
            except KeyError:
                e = e.p
        raise NameError(str(x)) from None

    if t is list:
        op, args = x
        if isinstance(op, Symbol):
            ## inline env.get
            e = r.env
            while e is not SENTINEL:
                try:
                    op = e.d[op]
                    break
                except KeyError:
                    e = e.p
            else:
                raise NameError(str(op)) from None

            if getattr(op, "special", False):
                r.argl = args
                return op
    elif t is Lambda:
        op = x
        args = EL
    else:
        r.val = x
        return r.cont

    ## push_ce(); push(args)
    stack.s = [args, [r.env, [r.cont, stack.s]]]

    ## list or sym lookup
    if callable(op):
        r.val = op
        return leval_proc_done_
    if not isinstance(op, list):
        raise SyntaxError(f"expected list or proc, got {op!r}")
    r.cont = leval_proc_done_
    r.exp = op
    return leval_


def leval_proc_done_():
    proc = r.val
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    ## pop r.argl and r.env
    r.argl, s = stack.s
    r.env, s = s

    if r.argl is EL:
        ## pop r.cont
        r.cont, stack.s = s
        return proc

    if proc.special:
        ## pop r.cont
        r.cont, stack.s = s
        return proc

    ## inline old leval_setup() to avoid function call
    ## push proc, SENTINEL, env
    s = [r.env, [SENTINEL, [proc, s]]]
    r.exp, args = r.argl
    if args is EL:
        r.cont = leval_last_
    else:
        s = [args, s]
        r.cont = leval_next_
    stack.s = s
    return leval_


def leval_next_():
    ## pop args and r.env
    args, s = stack.s
    r.env, s = s
    ## push val and r.env
    s = [r.env, [r.val, s]]
    ## inline old leval_setup() to avoid function call
    r.exp, args = args
    if args is EL:
        r.cont = leval_last_
    else:
        ## push args
        s = [args, s]
        r.cont = leval_next_
    stack.s = s
    return leval_


def leval_last_():
    ## pop r.env
    r.env, s = stack.s
    args = [r.val, EL]
    while True:
        ## pop x
        x, s = s
        if x is SENTINEL:
            break
        args = [x, args]
    r.argl = args
    ## pop proc
    proc, s = s
    ## pop cont
    r.cont, stack.s = s
    if proc.ffi:
        r.exp = proc
        return do_ffi
    return proc


## }}}
## {{{ ffi support


def do_ffi():
    push(r.cont)
    push(r.exp)  ## proc

    if r.argl is EL:
        r.argl = []
        return ffi_args_done_
    r.cont = ffi_args_done_
    r.exp = r.argl
    return lisp_value_to_py_value_


def ffi_args_done_():
    proc = pop()
    r.cont = pop()
    r.exp = proc(r.val)
    return py_value_to_lisp_value_


def lisp_value_to_py_value(x):
    r.exp = x
    r.cont = land
    return trampoline(lisp_value_to_py_value_)


def lisp_value_to_py_value_():
    x = r.exp
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        r.val = x
        return r.cont
    push(r.cont)
    push([])
    return lv2pv_setup_(x)


def lv2pv_setup_(args):
    r.exp, args = args
    push(args)
    r.cont = lv2pv_next_
    return lisp_value_to_py_value_


def lv2pv_next_():
    args = pop()
    argl = pop()
    argl.append(r.val)
    if args is EL:
        r.val = argl
        return pop()
    push(argl)
    return lv2pv_setup_(args)


def py_value_to_lisp_value(x):
    r.cont = land
    r.exp = x
    return trampoline(py_value_to_lisp_value_)


def py_value_to_lisp_value_():
    x = r.exp
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return go(x)
    if not x:
        return go(EL)

    push(r.cont)
    push(Queue())
    return pv2lv_setup_(list(x))


def pv2lv_setup_(args):
    r.exp = args.pop(0)
    push(args)
    r.cont = pv2lv_next_
    return py_value_to_lisp_value_


def pv2lv_next_():
    args = pop()
    argl = pop()
    argl.enqueue(r.val)
    if not args:
        r.val = argl.head()
        return pop()
    push(argl)
    return pv2lv_setup_(args)


## }}}
## {{{ special forms


@spcl("cond")
def op_cond():
    args = r.argl
    if args is EL:
        return go(EL)
    push(r.cont)
    return op_cond_setup_(args)


def op_cond_setup_(args):
    pc, args = args
    r.argl = pc
    r.exp, c = unpack2()
    push(c)
    push(r.env)
    if args is EL:
        r.cont = op_cond_last_
    else:
        push(args)
        r.cont = op_cond_next_
    return leval_


def op_cond_next_():
    args = pop()
    r.env = pop()
    r.exp = pop()
    if r.val is not EL:
        r.cont = pop()
        return leval_
    return op_cond_setup_(args)


def op_cond_last_():
    r.env = pop()
    r.exp = pop()
    r.cont = pop()
    if r.val is EL:
        return go(EL)
    return leval_


@spcl("define")
def op_define():
    sym, value = unpack2()
    push(symcheck(sym))
    push_ce()
    r.cont = op_define_
    r.exp = value
    return leval_


def op_define_():
    pop_ce()
    r.env.set(pop(), r.val)
    return go(EL)


@spcl("if")
def op_if():
    r.exp, c, a = unpack3()
    push_ce()
    push([c, a])
    r.cont = op_if_
    return leval_


def op_if_():
    c, a = pop()
    pop_ce()
    r.exp = a if r.val is EL else c
    return leval_


@spcl("lambda")
def op_lambda():
    params, body = unpack2()
    return go(Lambda(params, body, r.env))


@spcl("quote")
def op_quote():
    r.val = unpack1()
    return r.cont


@spcl("set!")
def op_setbang():
    sym, value = unpack2()
    push(symcheck(sym))
    push_ce()
    r.cont = op_setbang_
    r.exp = value
    return leval_


def op_setbang_():
    pop_ce()
    sym = pop()
    r.env.setbang(sym, r.val)
    return go(EL)


@spcl("special")
def op_special():
    sym, value = unpack2()
    push(symcheck(sym))
    push_ce()
    r.cont = op_special_
    r.exp = value
    return leval_


def op_special_():
    pop_ce()
    sym = pop()
    if not isinstance(r.val, Lambda):
        raise TypeError(f"expected lambda, got {r.val!r}")
    r.val.special = True
    r.env.set(sym, r.val)
    return go(EL)


@spcl("trap")
def op_trap():
    x = unpack1()
    ok = T
    push_ce()
    try:
        res = leval(x, r.env)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    pop_ce()
    return go([ok, [res, EL]])


## }}}
## {{{ quasiquote


@spcl("quasiquote")
def op_quasiquote():
    r.exp = unpack1()
    return qq_


def qq_():
    form = r.exp
    if not isinstance(form, list):
        return go(form)
    app = form[0]
    if eq(app, symbol("quasiquote")):
        ## XXX proper nesting?
        r.argl = form[1]
        return op_quasiquote
    if eq(app, symbol("unquote")):
        r.argl = form
        _, r.exp = unpack2()
        return leval_
    if eq(app, symbol("unquote-splicing")):
        _, __ = unpack2()
        raise LispError("cannot use unquote-splicing here")
    push_ce()
    push(SENTINEL)
    return qq_setup_(form)


def qq_setup_(form):
    elt, form = form
    if not (isinstance(form, list) or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    push(form)
    push_ce()
    if isinstance(elt, list) and eq(elt[0], symbol("unquote-splicing")):
        r.argl = elt
        _, r.exp = unpack2()
        r.cont = qq_spliced_
        return leval_
    r.cont = qq_next_
    r.exp = elt
    return qq_


def qq_spliced_():
    pop_ce()
    form = pop()
    value = r.val
    if value is EL:
        if form is EL:
            return qq_finish_
        return qq_setup_(form)
    while value is not EL:
        elt, value = value
        if value is EL:
            r.val = elt
            push(form)
            push_ce()
            return qq_next_
        push(elt)
    raise RuntimeError("bugs in the taters")


def qq_next_():
    pop_ce()
    form = pop()
    push(r.val)
    if form is EL:
        return qq_finish_
    return qq_setup_(form)


def qq_finish_():
    ret = EL
    while True:
        x = pop()
        if x is SENTINEL:
            break
        ret = [x, ret]
    pop_ce()
    r.val = ret
    return r.cont


## }}}
## {{{ primitives


def unary(f):
    args = r.argl
    if args is EL:
        raise SyntaxError("not enough args, need 2")
    x, args = args
    if args is not EL:
        raise SyntaxError("too many args, need 1")
    r.val = f(x)
    return r.cont


def binary(f):
    args = r.argl
    if args is EL:
        raise SyntaxError("not enough args, need 2")
    x, args = args
    if args is EL:
        raise SyntaxError("not enough args, need 2")
    y, args = args
    if args is not EL:
        raise SyntaxError("too many args, need 2")
    r.val = f(x, y)
    return r.cont


@glbl("apply")
def op_apply():
    proc, args = unpack2()
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    r.argl = args
    return proc


@glbl("atom?")
def op_atom():
    def f(x):
        return T if is_atom(x) else EL

    return unary(f)


@glbl("call/cc")
@glbl("call-with-current-contination")
def op_callcc():
    x = unpack1()
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")
    r.argl = [Continuation(r.cont), EL]
    return x


@glbl("car")
def op_car():
    def f(x):
        return listcheck(x)[0]

    return unary(f)


@glbl("cdr")
def op_cdr():
    def f(x):
        if x is EL:
            return x
        return listcheck(x)[1]

    return unary(f)


@glbl("cons")
def op_cons():
    r.val = list(unpack2())
    return r.cont


@glbl("div")
def op_div():
    def f(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(f)


@glbl("do")
def op_do():
    x = r.argl
    r.val = EL
    while x is not EL:
        if not isinstance(x, list):
            raise TypeError(f"expected list, got {x!r}")
        r.val, x = x
    return r.cont


@glbl("eq?")
def op_eq():
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(f)


@glbl("equal?")
def op_equal():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} {y!r}")
        return T if x == y else EL

    return binary(f)


@glbl("error")
def op_error():
    x = unpack1()
    raise LispError(x)


@glbl("eval")
def op_eval():
    args = r.argl
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
    e = r.env
    for _ in range(n_up):
        e = e.up()
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
    r.exp = x
    r.env = e
    return leval_


@glbl("exit")
def op_exit():
    x = unpack1()
    if isinstance(x, int):
        raise SystemExit(x)
    r.exp = x
    r.cont = op_exit_
    return stringify_


def op_exit_():
    raise SystemExit(r.val)


@glbl("last")
def op_last():
    x = unpack1()
    ret = EL
    while x is not EL:
        ret, x = x
    r.val = ret
    return r.cont


@glbl("lt?")
def op_lt():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return T if x < y else EL

    return binary(f)


@glbl("mul")
def op_mul():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x * y

    return binary(f)


@glbl("nand")
def op_nand():
    def f(x, y):
        if not (isinstance(x, int) and isinstance(y, int)):
            raise TypeError(f"expected integers, got {x!r} and {y!r}")
        return ~(x & y)

    return binary(f)


@glbl("null?")
def op_null():
    x = unpack1()
    r.val = T if x is EL else EL
    return r.cont


@glbl("print")
def op_print():
    args = r.argl

    if args is EL:
        print()
        r.val = EL
        return r.cont

    arg, args = args

    push(r.cont)
    push(args)
    r.exp = arg
    r.cont = op_print_
    return stringify_


def op_print_():
    args = pop()

    if args is EL:
        print(r.val)
        r.val = EL
        return pop()

    print(r.val, end=" ")

    arg, args = args

    push(args)
    r.exp = arg
    r.cont = op_print_
    return stringify_


@glbl("set-car!")
def op_setcarbang():
    def f(x, y):
        listcheck(x)[0] = y

    return binary(f)


@glbl("set-cdr!")
def op_setcdrbang():
    def f(x, y):
        listcheck(x)[1] = y

    return binary(f)


@glbl("sub")
def op_sub():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x - y

    return binary(f)


@glbl("type")
def op_type():
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

    return unary(f)


@glbl("while")
def op_while():
    x = unpack1()
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    push(r.cont)
    push(x)
    push(r.env)
    r.exp = x
    r.cont = op_while_
    return leval_


def op_while_():
    r.env = pop()
    x = top()

    if r.val is EL:
        pop()  ## x
        return pop()
    push(r.env)
    r.exp = x
    r.cont = op_while_
    return leval_


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


parse(RUNTIME, leval)


## }}}


if __name__ == "__main__":
    main()


## EOF
