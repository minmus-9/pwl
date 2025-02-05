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
lisp.py - lisp with continuations and "heap-based" recursion, builds on
lisp02/lisp.py
"""

## pylint: disable=invalid-name,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import sys
import traceback

## {{{ Struct (dumb)


class Struct:
    ## pylint: disable=too-few-public-methods

    def __init__(self, *frames, **kw):
        for frame in frames:
            self.__dict__.update(frame.__dict__)
        self.__dict__.update(kw)


## }}}
## {{{ trampoline


def trampoline(func, *args):
    while True:
        result = func(*args)
        assert isinstance(result, tuple) and tuple
        if len(result) == 1:
            return result[0]
        assert len(result) == 2
        func, args = result


def bounce(func, *args):
    return func, args


def land(value):
    return (value,)


## }}}
## {{{ basic defs

T = True
EL = object()


class Symbol(str):
    ...


## }}}
## {{{ symbol table

SYMBOLS = {}  ## global symbol table


def symbol(s):
    assert s and type(s) is str  ## pylint: disable=unidiomatic-typecheck
    return SYMBOLS.setdefault(s, Symbol(s))  ## mildly wasteful


## }}}
## {{{ type stuff


def ltype(x):
    ## pylint: disable=too-many-return-statements
    if x is T:
        return symbol("true")
    if x is EL:
        return symbol("false")
    if isinstance(x, Lambda):
        return symbol("lambda")
    if isinstance(x, Symbol):
        return symbol("symbol")
    if isinstance(x, int):
        return symbol("integer")
    if isinstance(x, float):
        return symbol("real")
    if isinstance(x, str):
        return symbol("string")
    if not isinstance(x, list):
        return symbol("primitive" if callable(x) else "opaque")
    if isinstance(x, Continuation):
        return symbol("continuation")
    listcheck(x)
    return symbol("list")


def syntaxcheck(boolean, msg):  ## acts sorta like assert
    if not boolean:
        raise SyntaxError(msg)


def listcheck(x):
    if not (
        isinstance(x, list)
        and len(x) == 2
        and (isinstance(x[1], list) or x[1] is EL)
    ):
        if x is EL:
            raise TypeError("expected list, got ()")
        raise TypeError(f"expected list, got {x!r}")
    return x


def symcheck(x):
    if not isinstance(x, Symbol):
        raise TypeError(f"expected symbol, got {stringify(x)}")
    return x


## }}}
## {{{ list ops and stuff


def car(x):
    return listcheck(x)[0]


def cdr(x):
    return EL if x is EL else listcheck(x)[1]  ## problem solved!


def set_car(lst, x):
    listcheck(lst)[0] = x
    return EL


def set_cdr(lst, lst2):
    if lst2 is not EL:
        listcheck(lst2)
    listcheck(lst)[1] = lst2
    return EL


def cons(x, y):
    if y is not EL:
        listcheck(y)
    return [x, y]


class ListBuilder:
    def __init__(self):
        self.h, self.t = EL, EL

    def append(self, x):
        node = cons(x, EL)
        if self.h is EL:
            self.h = node
        else:
            set_cdr(self.t, node)
        self.t = node

    def get(self):
        return self.h


def unpack(lst, n):
    ret = []
    for _ in range(n):
        if lst is EL:
            raise TypeError(f"not enough args, expected {n}")
        ret.append(car(lst))
        lst = cdr(lst)
    if lst is not EL:
        raise TypeError(f"too many args, expected {n}")
    return ret


## }}}
## {{{ environment


class Environment(dict):
    def __init__(self, params, args, parent):
        super().__init__()
        self.parent = parent
        variadic = False
        while params is not EL:
            p, params = symcheck(car(params)), cdr(params)
            if p is symbol("&"):
                variadic = True
            elif variadic:
                syntaxcheck(params is EL, "too many params after '&'")
                self[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                self[p], args = car(args), cdr(args)
        syntaxcheck(not variadic, "args end with '&'")
        syntaxcheck(args is EL, "too many args")

    def find(self, k):  ## NB caller ensures k is a Symbol
        d = self
        while d is not None:
            if k in d:
                return d
            d = d.parent
        raise NameError(k)


## }}}
## {{{ list-based frame stack


## NB list-based for continuations!
STACK = EL


def fpop():
    global STACK  ## pylint: disable=global-statement
    if STACK is EL:
        raise ValueError("stack empty")
    ret, STACK = car(STACK), cdr(STACK)
    return ret


def fpush(*frames, **kw):
    global STACK  ## pylint: disable=global-statement
    STACK = cons(Struct(*frames, **kw), STACK)


def freset():
    global STACK  ## pylint: disable=global-statement
    STACK = EL


def frestore(state):
    global STACK  ## pylint: disable=global-statement
    STACK = state


def fsave():
    ## if we used py list/append/pop we'd have to slice an
    ## entire copy for a continuation. this way, we don't
    ## have to do anything
    return STACK


def ftop():
    return car(STACK)


## }}}
## {{{ special forms

SPECIALS_ = Environment(EL, EL, None)
SPECIALS = Environment(EL, EL, SPECIALS_)  ## user modifies this


def spcl(name):
    def wrap(func):
        SPECIALS_[symbol(name)] = func
        return func

    return wrap


def op_cond_setup(frame, args):
    head, args = car(args), cdr(args)
    predicate, consequent = unpack(head, 2)

    fpush(frame, x=args, consequent=consequent)
    return bounce(leval_, Struct(frame, c=op_cond_cont, x=predicate))


def op_cond_cont(value):
    frame = fpop()
    args = frame.x

    if value is not EL:
        return bounce(leval_, Struct(frame, x=frame.consequent))
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
    frame = fpop()
    frame.e[symcheck(frame.sym)] = value
    return bounce(frame.c, EL)


@spcl("define")
def op_define(frame):
    sym, defn = unpack(frame.x, 2)

    fpush(frame, sym=sym)
    return bounce(leval_, Struct(frame, x=defn, c=op_define_cont))


@spcl("lambda")
def op_lambda(frame):
    params, body = unpack(frame.x, 2)

    if params is not EL:
        listcheck(params)
    return bounce(frame.c, Lambda(params, body, frame.e))


@spcl("quote")
def op_quote(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, x)


def op_setbang_cont(value):
    frame = fpop()
    frame.e.find(symcheck(frame.sym))[frame.sym] = value
    return bounce(frame.c, EL)


@spcl("set!")
def op_setbang(frame):
    sym, defn = unpack(frame.x, 2)

    fpush(frame, sym=sym)
    return bounce(leval_, Struct(frame, x=defn, c=op_setbang_cont))


def op_special_cont(value):
    if not isinstance(value, Lambda):
        ## NB these will all be recursive on stringify. this may be
        ##    ok because ops execute synchronously. there is a small
        ##    chance of blowing the python stack, but assuming that
        ##    risk beats the heck out of cps error messages.
        raise TypeError(f"expected lambda, got {stringify(value)}")
    frame = fpop()
    value.special = True
    frame.e[symcheck(frame.sym)] = value
    return bounce(frame.c, EL)


@spcl("special")
def op_special(frame):
    sym, defn = unpack(frame.x, 2)

    fpush(frame, sym=sym)
    return bounce(leval_, Struct(frame, x=defn, c=op_special_cont))


@spcl("trap")
def op_trap(frame):
    (x,) = unpack(frame.x, 1)
    ok = T
    try:
        ## this has to be recursive because you can't pass
        ## exceptions across the trampoline. you could
        ## redo the trampoline to handle this, but it'd be
        ## fugly and uninformative. better to say that there
        ## is some small chance that the recursive eval call
        ## blow the python stack in ~rare cases.
        res = leval(x, frame.e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return bounce(frame.c, cons(ok, cons(res, EL)))


## }}}
## {{{ built-in operators


class LispError(Exception):
    ...


GLOBALS_ = Environment(EL, EL, None)
GLOBALS = Environment(EL, EL, GLOBALS_)  ## user modifies this

GLOBALS_[symbol("#t")] = T


def glbl(name):
    def wrap(func):
        GLOBALS_[symbol(name)] = func
        return func

    return wrap


def unary(frame, func):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, func(x))


def binary(frame, func):
    x, y = unpack(frame.x, 2)
    return bounce(frame.c, func(x, y))


@glbl("atom?")
def op_atom(frame):
    return unary(
        frame,
        lambda x: T
        if ((x is T) or (x is EL) or isinstance(x, Symbol))
        else EL,
    )


@glbl("call/cc")
@glbl("call-with-current-continuation")
def op_callcc(frame):
    "(call/cc (lambda (cc) ...))"
    (x,) = unpack(frame.x, 1)
    if not callable(x):
        raise TypeError(f"call/cc expects callable, got {stringify(x)}")

    ## this is really all there is to it. really.
    continuation = Continuation(frame.c)

    arg = cons(continuation, EL)
    return bounce(x, Struct(frame, x=arg))


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
    def div(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(frame, div)


@glbl("eq?")
def op_eq(frame):
    def eq(x, y):
        if (x is T) or (x is EL) or isinstance(x, Symbol):
            return T if x is y else EL
        return EL

    return binary(frame, eq)


@glbl("equal?")
def op_equal(frame):
    return binary(frame, lambda x, y: T if x == y else EL)


@glbl("error")
def op_error(args, _):
    (x,) = unpack(args, 1)
    raise LispError(x)


@glbl("eval")
def op_eval(frame):
    (x,) = unpack(frame.x, 1)
    if isinstance(x, str) and not isinstance(x, Symbol):
        l = []
        p = Parser(l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    return bounce(leval_, Struct(frame, x=x))


def op_exit_cont(value):
    raise SystemExit(value)


@glbl("exit")
def op_exit(frame):
    (x,) = unpack(frame.x, 1)
    if isinstance(x, int):
        raise SystemExit(x)
    return bounce(stringify_, Struct(frame, x=x, c=op_exit_cont))


@glbl("lt?")
def op_lt(frame):
    return binary(frame, lambda x, y: T if x < y else EL)


@glbl("mul")
def op_mul(frame):
    return binary(frame, lambda x, y: x * y)


@glbl("nand")
def op_nand(frame):
    return binary(frame, lambda x, y: ~(x & y))


def op_print_cont(value):
    frame = fpop()
    args = frame.x
    if args is EL:
        print(value)
        return bounce(frame.c, EL)

    arg, args = car(args), cdr(args)
    sep = "" if args is EL else " "
    print(value, end=sep)

    fpush(frame, x=args)
    return bounce(stringify_, Struct(frame, x=arg, c=op_print_cont))


@glbl("print")
def op_print(frame):
    args = frame.x
    if args is EL:
        print()
        return bounce(frame.c, EL)

    arg, args = car(args), cdr(args)

    fpush(frame, x=args)
    return bounce(stringify_, Struct(frame, x=arg, c=op_print_cont))


@glbl("set-car!")
def op_setcarbang(frame):
    return binary(frame, set_car)


@glbl("set-cdr!")
def op_setcdrbang(frame):
    return binary(frame, set_cdr)


@glbl("sub")
def op_sub(frame):
    return binary(frame, lambda x, y: x - y)


@glbl(">string")
def op_tostring(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(stringify_, Struct(frame, x=x))


@glbl(">symbol")
def op_tosymbol(frame):
    (x,) = unpack(frame.x, 1)
    if not isinstance(x, str):
        raise TypeError(f"expected sym or str, got {stringify(x)}")
    return bounce(frame.c, symbol(x))


@glbl("type")
def op_type(frame):
    return unary(frame, ltype)


## }}}
## {{{ lambda


def lambda_body_done(bodystr):
    frame = fpop()
    paramstr = frame.x
    return bounce(frame.c, "(lambda " + paramstr + " " + bodystr + ")")


def lambda_params_done(paramstr):
    frame = fpop()
    body = frame.x

    fpush(frame, x=paramstr)
    return bounce(stringify_, Struct(frame, x=body, c=lambda_body_done))


class Lambda:
    ## pylint: disable=too-few-public-methods

    special = False

    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, frame):
        args = frame.x
        parent = frame.e if self.special else self.env  ## specials are weird
        e = Environment(self.params, args, parent)
        return bounce(leval_, Struct(frame, x=self.body, e=e))

    def as_str_(self, frame):
        fpush(frame, x=self.body)
        return bounce(
            stringify_, Struct(frame, x=self.params, c=lambda_params_done)
        )


## }}}
## {{{ continuations


class Continuation:
    ## pylint: disable=too-few-public-methods

    def __init__(self, continuation):
        self.continuation = continuation  ## a python func
        self.stack = fsave()

    def __call__(self, frame):
        (x,) = unpack(frame.x, 1)
        frestore(self.stack)
        return bounce(self.continuation, x)  ## that's it.


## }}}
## {{{ stringify


def stringify(x, env=GLOBALS):
    return trampoline(stringify_, Struct(x=x, c=land, e=env))


def stringify_setup(frame, args):
    arg, args = car(args), cdr(args)
    fpush(frame, x=args)
    return bounce(stringify_, Struct(frame, x=arg, c=stringify_cont))


def stringify_cont(value):
    frame = fpop()
    args = frame.x

    if args is EL:
        parts = [value]
        while True:
            f = fpop()
            if f.x is None:
                break
            parts.insert(0, f.x)
        return bounce(frame.c, "(" + " ".join(parts) + ")")

    fpush(frame, x=value)
    return stringify_setup(frame, args)


def stringify_(frame):
    ## pylint: disable=too-many-return-statements
    x = frame.x
    if x is T:
        return bounce(frame.c, "#t")
    if x is EL:
        return bounce(frame.c, "()")
    if isinstance(x, (Symbol, int, float)):  ## check Symbol here...
        return bounce(frame.c, str(x))
    if isinstance(x, str):  ## ... and str here
        return bounce(frame.c, '"' + repr(x)[1:-1].replace('"', '\\"') + '"')
    if isinstance(x, Lambda):
        return bounce(x.as_str_, Struct(frame, x=x))
    if isinstance(x, Continuation):
        return bounce(frame.c, "[continuation]")
    if not isinstance(x, list):
        z = "primitive" if callable(x) else "opaque"
        return bounce(frame.c, z)  ## python func
    listcheck(x)

    fpush(frame, x=None)  ## sentinel

    return stringify_setup(frame, x)


## }}}
## {{{ eval


def leval(x, env=GLOBALS):
    return trampoline(leval_, Struct(x=x, c=land, e=env))


def leval_setup(frame, args):
    arg, args = car(args), cdr(args)
    fpush(frame, x=args)
    return bounce(leval_, Struct(frame, x=arg, c=leval_next_arg))


def leval_next_arg(value):
    frame = fpop()
    args = frame.x

    if args is EL:
        ret = cons(value, EL)
        while True:
            f = fpop()
            if f.x is None:
                proc = f.proc
                break
            ret = cons(f.x, ret)
        return bounce(proc, Struct(frame, x=ret))

    fpush(frame, x=value)
    return leval_setup(frame, args)


def leval_proc_done(proc):
    frame = fpop()
    args = frame.x

    if not callable(proc):  ## python func or Lambda
        raise TypeError(proc)

    if getattr(proc, "special", False):
        return bounce(proc, frame)
    if args is EL:
        return bounce(proc, frame)

    fpush(frame, proc=proc, x=None)  ## None is sentinel

    return leval_setup(frame, args)


def leval_(frame):
    x, env = frame.x, frame.e
    if isinstance(x, Symbol):  ## test Symbol *before* str
        return bounce(frame.c, env.find(x)[x])
    if (x is T) or (x is EL) or isinstance(x, (int, float, str)):
        return bounce(frame.c, x)
    sym, args = car(listcheck(x)), cdr(x)
    if isinstance(sym, Symbol):
        try:
            op = SPECIALS.find(sym)[sym]
        except NameError:
            pass
        else:
            return bounce(op, Struct(frame, x=args))
    else:
        listcheck(sym)

    fpush(frame, x=args)
    return bounce(leval_, Struct(frame, x=sym, c=leval_proc_done))


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
            self.stack.append(ListBuilder())
        elif ttype == self.scanner.T_RPAR:
            syntaxcheck(self.stack, "too many ')'s")
            l = self.stack.pop().get()
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
            ## have to reset scanner/parser state and stack
            p = Parser(callback)
            freset()
            traceback.print_exception(*sys.exc_info())
        except:  ## pylint: disable=bare-except
            ## reset stack because we have no clue what just happened
            freset()
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
        if filename == "+":
            try:
                sys.set_int_max_str_digits(0)
            except AttributeError:
                pass
            continue
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
    try:
        main()
    finally:
        if STACK is not EL:
            print("S", STACK)


## EOF
