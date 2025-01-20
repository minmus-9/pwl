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
oo.py - builds on list.py and adds a working state class

thoughts
    - no point in changing the lisp object representation to class-based:
      that's a bit of work (easy.py vs rec.py). this is an oo version of
      lisp.py not a framework for writing lisps.
    - things like T and EL are global.
    - the symbol table should be global (see note below).
    - @glbl and @spcl are good things and should continue to work
      the same way. this means they stay global.
    - the global SPECAL and GLOBALS are used to seed the lisp namespace
    - there's no real gain to making primitives into methods. if you don't
      like one, redefine it. subclass Lisp so that the state argument to
      your primitive (which is a Lisp) has your goodies in it. then you can
      use @glbl and @spcl in a class and still get self as the first arg.
"""

## pylint: disable=invalid-name,unbalanced-tuple-unpacking,too-many-lines
## XXX pylint: disable=missing-docstring

import os
import sys
import threading
import traceback

__all__ = ("Lisp", "lisp")

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


def land(*args):
    return (args,)


## }}}
## {{{ basic defs

T = True


class _EL:
    ## pylint: disable=too-few-public-methods
    "just provides repr"

    def __repr__(self):
        return "EL"


EL = _EL()


class Symbol(str):
    ...


SENTINEL = object()


LOCK = threading.Lock()


## }}}
## {{{ symbol table

SYMBOLS = {}  ## global symbol table shared by all interpreters


def symbol_(_, s):
    assert s and type(s) is str  ## pylint: disable=unidiomatic-typecheck
    with LOCK:
        return SYMBOLS.setdefault(s, Symbol(s))  ## mildly wasteful


## speech: symbols should be global so that (eq? 'x 'x) is always true,
## even across interpreters. so we're not going to make everything into
## a class. we'll just thread-lock the symbol table instead.


## }}}
## {{{ type stuff


def ltype(state, x):
    ## pylint: disable=too-many-return-statements
    if x is T:
        return state.symbol("true")
    if x is EL:
        return state.symbol("false")
    if isinstance(x, Lambda):
        return state.symbol("lambda")
    if isinstance(x, Symbol):
        return state.symbol("symbol")
    if isinstance(x, int):
        return state.symbol("integer")
    if isinstance(x, float):
        return state.symbol("real")
    if isinstance(x, str):
        return state.symbol("string")
    if isinstance(x, Continuation):
        return state.symbol("continuation")
    if not isinstance(x, list):
        return state.symbol("primitive" if callable(x) else "opaque")
    listcheck(state, x)
    return state.symbol("list")


def syntaxcheck(boolean, msg):  ## acts sorta like assert
    if not boolean:
        raise SyntaxError(msg)


def listcheck(_, x):
    if not (
        isinstance(x, list)
        and len(x) == 2
        and (isinstance(x[1], list) or x[1] is EL)
    ):
        if x is EL:
            raise TypeError("expected list, got ()")
        raise TypeError(f"expected list, got {x!r}")
    return x


def symcheck(state, x):
    if not isinstance(x, Symbol):
        ## NB note use of state.globals
        raise TypeError(
            f"expected symbol, got {stringify(state, x, state.globals)}"
        )
    return x


## }}}
## {{{ list ops and stuff


def car(state, x):
    return listcheck(state, x)[0]


def cdr(state, x):
    return EL if x is EL else listcheck(state, x)[1]  ## problem solved!


def set_car(state, lst, x):
    listcheck(state, lst)[0] = x
    return EL


def set_cdr(state, lst, lst2):
    listcheck(state, lst)[1] = listcheck(state, lst2)
    return EL


def cons(state, x, y):
    if y is not EL:
        listcheck(state, y)
    return [x, y]


class ListBuilder:
    def __init__(self, state):
        self.state = state
        self.h, self.t = EL, EL

    def append(self, x):
        node = cons(self.state, x, EL)
        if self.h is EL:
            self.h = node
        else:
            set_cdr(self.state, self.t, node)
        self.t = node

    def get(self):
        return self.h


def unpack(state, lst, n):
    ret = []
    for _ in range(n):
        if lst is EL:
            raise TypeError(f"not enough args, expected {n}")
        ret.append(car(state, lst))
        lst = cdr(state, lst)
    if lst is not EL:
        raise TypeError(f"too many args, expected {n}")
    return ret


## }}}
## {{{ environment


class Environment(dict):
    def __init__(self, state, params, args, parent):
        super().__init__()
        self.state = state
        self.parent = parent
        variadic = False
        while params is not EL:
            p, params = symcheck(state, car(state, params)), cdr(
                state, params
            )
            if p is state.symbol("&"):
                variadic = True
            elif variadic:
                syntaxcheck(params is EL, "too many params after '&'")
                self[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                self[p], args = car(state, args), cdr(state, args)
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
## {{{ special forms

SPECIALS = {}


def spcl(name):
    def wrap(func):
        ## NB using state==None here!
        SPECIALS[symbol_(None, name)] = func
        return func

    return wrap


def op_cond_setup(state, frame, args):
    head, args = car(state, args), cdr(state, args)
    predicate, consequent = unpack(state, head, 2)

    state.fpush(frame, x=args, consequent=consequent)
    return bounce(leval_, state, Struct(frame, c=op_cond_cont, x=predicate))


def op_cond_cont(state, value):
    frame = state.fpop()
    args = frame.x

    if value is not EL:
        return bounce(leval_, state, Struct(frame, x=frame.consequent))
    if args is EL:
        return bounce(frame.c, state, EL)
    return op_cond_setup(state, frame, args)


@spcl("cond")
def op_cond(state, frame):
    args = frame.x
    if args is EL:
        return bounce(frame.c, state, EL)

    return op_cond_setup(state, frame, args)


def op_define_cont(state, value):
    frame = state.fpop()
    frame.e[symcheck(state, frame.sym)] = value
    return bounce(frame.c, state, EL)


@spcl("define")
def op_define(state, frame):
    sym, defn = unpack(state, frame.x, 2)

    state.fpush(frame, sym=sym)
    return bounce(leval_, state, Struct(frame, x=defn, c=op_define_cont))


@spcl("lambda")
def op_lambda(state, frame):
    params, body = unpack(state, frame.x, 2)

    if params is not EL:
        listcheck(state, params)
    return bounce(frame.c, state, Lambda(params, body, frame.e))


## this follows https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html
## (special) doesn't quite get the job done due to the way its env works.
## it ain't the same as a recursive scheme macro :-)

def qq_list(state, frame):
    form = frame.x
    app = car(state, form)

    if app is state.symbol("quasiquote"):
        _, x = unpack(state, form, 2)
        return qq(state, Struct(frame, x=x))

    if app is state.symbol("unquote"):
        _, x = unpack(state, form, 2)
        return leval(state, x, frame.e)

    if app is state.symbol("unquote-splicing"):
        _, x = unpack(state, form, 2)
        raise LispError("cannot use unquote-splicing here")

    state.fpush(frame, x=SENTINEL)
    while form is not EL:
        elt, form = car(state, form), cdr(state, form)
        if (
            isinstance(elt, list)
            and car(state, elt) is state.symbol("unquote-splicing")
        ):
            _, x = unpack(state, elt, 2)
            elts = leval(state, x, frame.e)
            listcheck(state, elts)
            while elts is not EL:
                elt, elts = car(state, elts), cdr(state, elts)
                state.fpush(frame, x=elt)
        else:
            elt = qq(state, Struct(frame, x=elt))
            state.fpush(frame, x=elt)
    res = EL
    while True:
        f = state.fpop()
        if f.x is SENTINEL:
            break
        res = cons(state, f.x, res)
    return res


def qq(state, frame):
    form = frame.x
    if isinstance(form, list):
        return qq_list(state, frame)
    return form


@spcl("quasiquote")
def op_quasiquote(state, frame):
    (form,) = unpack(state, frame.x, 1)
    return bounce(frame.c, state, qq(state, Struct(frame, x=form)))


@spcl("quote")
def op_quote(state, frame):
    (x,) = unpack(state, frame.x, 1)
    return bounce(frame.c, state, x)


def op_setbang_cont(state, value):
    frame = state.fpop()
    frame.e.find(symcheck(state, frame.sym))[frame.sym] = value
    return bounce(frame.c, state, EL)


@spcl("set!")
def op_setbang(state, frame):
    sym, defn = unpack(state, frame.x, 2)

    state.fpush(frame, sym=sym)
    return bounce(leval_, state, Struct(frame, x=defn, c=op_setbang_cont))


def op_special_cont(state, value):
    if not isinstance(value, Lambda):
        ## NB these will all be recursive on stringify. this may be
        ##    ok because ops execute synchronously. there is a small
        ##    chance of blowing the python stack, but assuming that
        ##    risk beats the heck out of cps error messages.
        ##
        ## NB note use of state.globals
        raise TypeError(
            f"expected lambda, got {stringify(state, value, state.globals)}"
        )
    frame = state.fpop()
    value.special = True
    frame.e[symcheck(state, frame.sym)] = value
    return bounce(frame.c, state, EL)


@spcl("special")
def op_special(state, frame):
    sym, defn = unpack(state, frame.x, 2)

    state.fpush(frame, sym=sym)
    return bounce(leval_, state, Struct(frame, x=defn, c=op_special_cont))


@spcl("trap")
def op_trap(state, frame):
    (x,) = unpack(state, frame.x, 1)
    ok = T
    try:
        ## this has to be recursive because you can't pass
        ## exceptions across the trampoline. you could
        ## redo the trampoline to handle this, but it'd be
        ## fugly and uninformative. better to say that there
        ## is some small chance that the recursive eval call
        ## blow the python stack in ~rare cases.
        res = leval(state, x, frame.e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return bounce(frame.c, state, cons(state, ok, cons(state, res, EL)))


## }}}
## {{{ built-in operators


class LispError(Exception):
    ...


GLOBALS = {}

GLOBALS[symbol_(None, "#t")] = T


def glbl(name):
    def wrap(func):
        GLOBALS[symbol_(None, name)] = func
        return func

    return wrap


def unary(state, frame, func):
    (x,) = unpack(state, frame.x, 1)
    return bounce(frame.c, state, func(x))


def binary(state, frame, func):
    x, y = unpack(state, frame.x, 2)
    return bounce(frame.c, state, func(x, y))


@glbl("atom?")
def op_atom(state, frame):
    return unary(
        state,
        frame,
        lambda x: T
        if ((x is T) or (x is EL) or isinstance(x, Symbol))
        else EL,
    )


@glbl("call/cc")
@glbl("call-with-current-continuation")
def op_callcc(state, frame):
    "(call/cc (lambda (cc) ...))"
    (x,) = unpack(state, frame.x, 1)
    if not callable(x):
        raise TypeError(
            f"call/cc expects callable, got {stringify(state, x, state.globals)}"
        )

    ## this is really all there is to it. really.
    cc = Continuation(state, frame.c)

    arg = cons(state, cc, EL)
    return bounce(x, state, Struct(frame, x=arg))


@glbl("car")
def op_car(state, frame):
    return unary(state, frame, lambda x: car(state, x))


@glbl("cdr")
def op_cdr(state, frame):
    return unary(state, frame, lambda x: cdr(state, x))


@glbl("cons")
def op_cons(state, frame):
    return binary(state, frame, lambda x, l: cons(state, x, l))


@glbl("div")
def op_div(state, frame):
    def div(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(state, frame, div)


@glbl("eq?")
def op_eq(state, frame):
    def eq(x, y):
        if (x is T) or (x is EL) or isinstance(x, Symbol):
            return T if x is y else EL
        return EL

    return binary(state, frame, eq)


@glbl("equal?")
def op_equal(state, frame):
    return binary(state, frame, lambda x, y: T if x == y else EL)


@glbl("error")
def op_error(state, frame):
    (x,) = unpack(state, frame.x, 1)
    raise LispError(x)


@glbl("eval")
def op_eval(state, frame):
    (x,) = unpack(state, frame.x, 1)
    if isinstance(x, str) and not isinstance(x, Symbol):
        l = []
        p = Parser(state, l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    return bounce(leval_, state, Struct(frame, x=x))


def op_exit_cont(_, value):
    raise SystemExit(value)


@glbl("exit")
def op_exit(state, frame):
    (x,) = unpack(state, frame.x, 1)
    if isinstance(x, int):
        raise SystemExit(x)
    return bounce(stringify_, state, Struct(frame, x=x, c=op_exit_cont))


@glbl("lt?")
def op_lt(state, frame):
    return binary(state, frame, lambda x, y: T if x < y else EL)


@glbl("mul")
def op_mul(state, frame):
    return binary(state, frame, lambda x, y: x * y)


@glbl("nand")
def op_nand(state, frame):
    return binary(state, frame, lambda x, y: ~(x & y))


def op_print_cont(state, value):
    frame = state.fpop()
    args = frame.x
    if args is EL:
        print(value)
        return bounce(frame.c, state, EL)

    arg, args = car(state, args), cdr(state, args)
    print(value, end=" ")

    state.fpush(frame, x=args)
    return bounce(stringify_, state, Struct(frame, x=arg, c=op_print_cont))


@glbl("print")
def op_print(state, frame):
    args = frame.x
    if args is EL:
        print()
        return bounce(frame.c, state, EL)

    arg, args = car(state, args), cdr(state, args)

    state.fpush(frame, x=args)
    return bounce(stringify_, state, Struct(frame, x=arg, c=op_print_cont))


@glbl("set-car!")
def op_setcarbang(state, frame):
    return binary(state, frame, lambda l, x: set_car(state, l, x))


@glbl("set-cdr!")
def op_setcdrbang(state, frame):
    return binary(state, frame, lambda l, x: set_cdr(state, l, x))


@glbl("sub")
def op_sub(state, frame):
    return binary(state, frame, lambda x, y: x - y)


@glbl(">float")
def op_tofloat(state, frame):
    (x,) = unpack(state, frame.x, 1)
    return bounce(frame.c, state, float(x))


@glbl(">int")
def op_toint(state, frame):
    (x,) = unpack(state, frame.x, 1)
    return bounce(frame.c, state, int(x))


@glbl(">string")
def op_tostring(state, frame):
    (x,) = unpack(state, frame.x, 1)
    return bounce(stringify_, state, Struct(frame, x=x))


@glbl(">symbol")
def op_tosymbol(state, frame):
    (x,) = unpack(state, frame.x, 1)
    if not isinstance(x, str):
        raise TypeError(
            f"expected sym or str, got {stringify(state, x, state.globals)}"
        )
    return bounce(frame.c, state, state.symbol(x))


@glbl("type")
def op_type(state, frame):
    return unary(state, frame, lambda x: ltype(state, x))


@glbl("unquote")
def op_unquote(state, frame):
    (x,) = unpack(state, frame.x, 1)
    raise LispError("cannot unquote here")


@glbl("unquote-splicing")
def op_unquote_splicing(state, frame):
    (x,) = unpack(state, frame.x, 1)
    raise LispError("cannot unquote-splicing here")


# }}}
## {{{ lambda


def lambda_body_done(state, bodystr):
    frame = state.fpop()
    paramstr = frame.x
    return bounce(frame.c, state, "(lambda " + paramstr + " " + bodystr + ")")


def lambda_params_done(state, paramstr):
    frame = state.fpop()
    body = frame.x

    state.fpush(frame, x=paramstr)
    return bounce(
        stringify_, state, Struct(frame, x=body, c=lambda_body_done)
    )


class Lambda:
    ## pylint: disable=too-few-public-methods

    special = False

    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, state, frame):
        args = frame.x
        parent = frame.e if self.special else self.env  ## specials are weird
        e = Environment(state, self.params, args, parent)
        return bounce(leval_, state, Struct(frame, x=self.body, e=e))

    def as_str_(self, state, frame):
        state.fpush(frame, x=self.body)
        return bounce(
            stringify_,
            state,
            Struct(frame, x=self.params, c=lambda_params_done),
        )


## }}}
## {{{ continuations


class Continuation:
    ## pylint: disable=too-few-public-methods

    def __init__(self, state, continuation):
        self.state = state
        self.continuation = continuation  ## a python func
        self.stack = self.state.fsave()

    def __call__(self, state, frame):
        (x,) = unpack(state, frame.x, 1)
        self.state.frestore(self.stack)
        return bounce(self.continuation, self.state, x)  ## that's it.


## }}}
## {{{ stringify


def stringify(state, x, env):
    return trampoline(stringify_, state, Struct(x=x, c=land, e=env))[1]


def stringify_setup(state, frame, args):
    arg, args = car(state, args), cdr(state, args)
    state.fpush(frame, x=args)
    return bounce(stringify_, state, Struct(frame, x=arg, c=stringify_cont))


def stringify_cont(state, value):
    frame = state.fpop()
    args = frame.x

    if args is EL:
        parts = [value]
        while True:
            f = state.fpop()
            if f.x is SENTINEL:
                break
            parts.insert(0, f.x)
        return bounce(frame.c, state, "(" + " ".join(parts) + ")")

    state.fpush(frame, x=value)
    return stringify_setup(state, frame, args)


def stringify_(state, frame):
    ## pylint: disable=too-many-return-statements
    x = frame.x
    if x is T:
        return bounce(frame.c, state, "#t")
    if x is EL:
        return bounce(frame.c, state, "()")
    if isinstance(x, (Symbol, int, float)):  ## check Symbol here...
        return bounce(frame.c, state, str(x))
    if isinstance(x, str):  ## ... and str here
        return bounce(
            frame.c, state, '"' + repr(x)[1:-1].replace('"', '\\"') + '"'
        )
    if isinstance(x, Lambda):
        return bounce(x.as_str_, state, Struct(frame, x=x))
    if isinstance(x, Continuation):
        return bounce(frame.c, state, "[continuation]")
    if not isinstance(x, list):
        z = "[operator]" if callable(x) else "[opaque]"
        return bounce(frame.c, state, z)  ## python func
    listcheck(state, x)

    state.fpush(frame, x=SENTINEL)  ## sentinel

    return stringify_setup(state, frame, x)


## }}}
## {{{ eval


def leval(state, x, env):
    return trampoline(leval_, state, Struct(x=x, c=land, e=env))[1]


def leval_setup(state, frame, args):
    arg, args = car(state, args), cdr(state, args)
    state.fpush(frame, x=args)
    return bounce(leval_, state, Struct(frame, x=arg, c=leval_next_arg))


def leval_next_arg(state, value):
    frame = state.fpop()
    args = frame.x

    if args is EL:
        ret = cons(state, value, EL)
        while True:
            f = state.fpop()
            if f.x is SENTINEL:
                proc = f.proc
                break
            ret = cons(state, f.x, ret)
        return bounce(proc, state, Struct(frame, x=ret))

    state.fpush(frame, x=value)
    return leval_setup(state, frame, args)


def leval_proc_done(state, proc):
    frame = state.fpop()
    args = frame.x

    if not callable(proc):  ## python func or Lambda
        raise TypeError(proc)

    if getattr(proc, "special", False):
        return bounce(proc, state, frame)
    if args is EL:
        return bounce(proc, state, frame)

    state.fpush(frame, proc=proc, x=SENTINEL)  ## sentinel

    return leval_setup(state, frame, args)


def leval_(state, frame):
    x, env = frame.x, frame.e
    if isinstance(x, Symbol):  ## test Symbol *before* str
        return bounce(frame.c, state, env.find(x)[x])
    if (x is T) or (x is EL) or isinstance(x, (int, float, str)):
        return bounce(frame.c, state, x)
    if callable(x) and not isinstance(x, (Lambda, Continuation)):
        return bounce(frame.c, state, x)
    sym, args = car(state, listcheck(state, x)), cdr(state, x)
    if isinstance(sym, Symbol):
        try:
            op = state.specials.find(sym)[sym]
        except NameError:
            pass
        else:
            return bounce(op, state, Struct(frame, x=args))
    elif not callable(sym):
        listcheck(state, sym)
    else:
        state.fpush(frame, proc=sym, x=args)
        return leval_proc_done(state, sym)

    state.fpush(frame, x=args)
    return bounce(leval_, state, Struct(frame, x=sym, c=leval_proc_done))


## }}}
## {{{ lisp->python ffi


FFI_REGISTRY = {}


def ffi(name=None):
    def wrap(func):
        n = name or func.__name__
        FFI_REGISTRY[n] = func
        return func

    return wrap


def lisp_value_to_py_value(state, x):
    return trampoline(
        lisp_value_to_py_value_, state, Struct(x=x, e=None, c=land)
    )[1]


def lv2pv_setup(state, frame, args):
    arg, args = car(state, args), cdr(state, args)
    state.fpush(frame, x=args)
    return bounce(
        lisp_value_to_py_value_, state, Struct(frame, x=arg, c=lv2pv_next_arg)
    )


def lv2pv_next_arg(state, value):
    frame = state.fpop()
    args = frame.x

    if args is EL:
        ret = [value]
        while True:
            f = state.fpop()
            if f.x is SENTINEL:
                break
            ret.insert(0, f.x)
        return bounce(frame.c, state, ret)

    state.fpush(frame, x=value)
    return lv2pv_setup(state, frame, args)


def lisp_value_to_py_value_(state, frame):
    x = frame.x
    if x is EL:
        x = None
    elif x is T:
        x = True
    elif isinstance(x, Symbol):
        x = str(x)
    try:
        listcheck(state, x)
    except TypeError:
        return bounce(frame.c, state, x)

    state.fpush(frame, x=SENTINEL)  ## sentinel
    return lv2pv_setup(state, frame, x)


def py_value_to_lisp_value(state, x):
    return trampoline(
        py_value_to_lisp_value_, state, Struct(x=x, e=None, c=land)
    )[1]


def pv2lv_setup(state, frame, args):
    arg, args = args[0], args[1:]
    state.fpush(frame, x=args)
    return bounce(
        py_value_to_lisp_value_, state, Struct(frame, x=arg, c=pv2lv_next_arg)
    )


def pv2lv_next_arg(state, value):
    frame = state.fpop()
    args = frame.x

    if not args:
        ret = cons(state, value, EL)
        while True:
            f = state.fpop()
            if f.x is SENTINEL:
                break
            ret = cons(state, f.x, ret)
        return bounce(frame.c, state, ret)

    state.fpush(frame, x=value)
    return pv2lv_setup(state, frame, args)


def py_value_to_lisp_value_(state, frame):
    x = frame.x
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return bounce(frame.c, state, x)
    if not x:
        return bounce(frame.c, state, EL)

    state.fpush(frame, x=SENTINEL)  ## sentinel
    return pv2lv_setup(state, frame, list(x))


def ffi_args_done(state, args):
    frame = state.fpop()
    func = frame.x
    ret = func(*args)
    return bounce(py_value_to_lisp_value_, state, Struct(frame, x=ret))


@glbl("ffi")
def op_py_ffi(state, frame):
    "(ffi func_name ...)"
    x = frame.x
    x, args = car(state, x), cdr(state, x)
    if isinstance(x, Symbol):
        x = str(x)
    elif not isinstance(x, str):
        raise TypeError(
            f"expected sym or str, got {stringify(state, x, state.globals)}"
        )
    func = FFI_REGISTRY.get(x)
    if func is None:
        raise ValueError(f"function {x!r} is unknown to ffi")

    state.fpush(frame, x=func)

    if args is EL:
        return bounce(ffi_args_done, state, [])

    return bounce(
        lisp_value_to_py_value_,
        state,
        Struct(frame, x=list(args), c=ffi_args_done),
    )


def lisp_list_to_py_list(state, lst):
    ret = []
    while lst is not EL:
        ret.append(car(state, lst))
        lst = cdr(state, lst)
    return ret


def py_list_to_lisp_list(state, lst):
    lb = ListBuilder(state)
    for x in lst:
        lb.append(x)
    return lb.get()


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

    def __init__(self, callback):
        self.callback = callback
        self.token = ""
        self.state = self.S_SYM

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
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
                syntaxcheck(not self.token, "tick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_TICK)
            elif ch == "`":
                syntaxcheck(not self.token, "backtick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_BACKTICK)
            elif ch == ",":
                syntaxcheck(not self.token, "comma is not a delimiter")
                self.state = self.S_COMMA
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
    def __init__(self, state, callback):
        self.state, self.callback = state, callback
        self.stack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.q_map = {
            self.state.symbol("'"): self.state.symbol("quote"),
            self.state.symbol("`"): self.state.symbol("quasiquote"),
            self.state.symbol(","): self.state.symbol("unquote"),
            self.state.symbol(",@"): self.state.symbol("unquote-splicing"),
        }

    def process_token(self, ttype, token):
        if ttype == self.scanner.T_SYM:
            self.add(self.state.symbol(token))
        elif ttype == self.scanner.T_LPAR:
            self.stack.append(ListBuilder(self.state))
        elif ttype == self.scanner.T_RPAR:
            syntaxcheck(self.stack, "too many ')'s")
            l = self.filter(self.stack.pop().get())
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
        elif ttype == self.scanner.T_TICK:
            self.add(self.state.symbol("'"))
        elif ttype == self.scanner.T_BACKTICK:
            self.add(self.state.symbol("`"))
        elif ttype == self.scanner.T_COMMA:
            self.add(self.state.symbol(","))
        elif ttype == self.scanner.T_COMMA_AT:
            self.add(self.state.symbol(",@"))
        elif ttype == self.scanner.T_EOF:
            syntaxcheck(not self.stack, "premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        syntaxcheck(
            self.stack,
            f"expected '(' got {stringify(self.state, x, self.state.globals)}",
        )
        self.stack[-1].append(x)

    def filter(self, sexpr):
        ## pylint: disable=no-self-use
        "process ' ` , ,@"
        lb = ListBuilder(self.state)
        while sexpr is not EL:
            elt, sexpr = car(self.state, sexpr), cdr(self.state, sexpr)
            if isinstance(elt, Symbol) and elt in self.q_map:
                elt, sexpr = self.process_syms(elt, sexpr)
            lb.append(elt)
        return lb.get()

    def process_syms(self, elt, sexpr):
        replacement = self.q_map[elt]
        if sexpr is EL:
            raise SyntaxError(f"got {elt!r} at end of list")
        quoted, sexpr = car(self.state, sexpr), cdr(self.state, sexpr)
        if isinstance(quoted, Symbol) and quoted in self.q_map:
            ## NB this is recursive but likely ok
            quoted, sexpr = self.process_syms(quoted, sexpr)
        elt = cons(self.state, replacement, cons(self.state, quoted, EL))
        return elt, sexpr


## }}}
## {{{ main repl


def repl(state, callback):
    try:
        import readline as _  ## pylint: disable=import-outside-toplevel
    except ImportError:
        pass

    ## pylint: disable=unused-variable
    p, rc, stop = Parser(state, callback), 0, False

    def feed(x):
        nonlocal p, rc, stop
        try:
            p.feed(x)
        except SystemExit as exc:
            stop, rc = True, exc.args[0]
        except SyntaxError:
            ## have to reset scanner/parser state and stack
            p = Parser(state, callback)
            state.freset()
            traceback.print_exception(*sys.exc_info())
        except:  ## pylint: disable=bare-except
            ## reset stack because we have no clue what just happened
            state.freset()
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
            value = leval(lisp, sexpr, lisp.globals)
        except:
            print("Offender (pyth):", sexpr)
            print(
                "Offender (lisp):",
                stringify(lisp, sexpr, lisp.globals),
                "\n",
            )
            raise
        if value is not EL:
            print(stringify(lisp, value, lisp.globals))

    def eat(src):
        p = Parser(lisp, callback)
        p.feed(src)
        p.feed(None)

    def load(filename):
        for base in [os.getcwd(), os.path.dirname(__file__)] + sys.path:
            path = os.path.join(base, filename)
            if os.path.exists(path):
                filename = path
                break
        else:
            raise RuntimeError(f"cannot find {filename}")
        with open(  ## pylint: disable=unspecified-encoding
            filename, "r"
        ) as fp:
            eat(fp.read())

    stop = True
    for filename in sys.argv[1:]:
        if filename == "+":
            try:
                sys.set_int_max_str_digits(0)
            except AttributeError:
                pass
            continue
        if filename == "-":
            stop = False
            break
        load(filename)
        stop = True
    if force_repl or not stop:
        raise SystemExit(repl(lisp, callback))


## }}}
## {{{ lisp class


class Lisp:
    ## pylint: disable=too-many-public-methods

    def __init__(self):
        self.stack = EL  ## runtime stack
        self.specials_ = Environment(self, EL, EL, None)
        self.specials_.update(SPECIALS)
        self.specials = Environment(self, EL, EL, self.specials_)
        self.globals_ = Environment(self, EL, EL, None)
        self.globals_.update(GLOBALS)
        self.globals = Environment(self, EL, EL, self.globals_)

    ########################################################################
    ### public (non-development) api

    error = LispError = LispError
    T = T
    EL = EL

    def car(self, l):
        return car(self, l)

    def cdr(self, l):
        return cdr(self, l)

    def cons(self, l, x):
        return cons(self, l, x)

    def set_car(self, l, x):
        return set_car(self, l, x)

    def set_cdr(self, l, x):
        return set_cdr(self, l, x)

    def stringify(self, x, env=None):
        return stringify(self, x, self.globals if env is None else env)

    def symbol(self, s):
        return symbol_(self, s)

    ### high level

    def call(self, obj, *args):
        if isinstance(obj, str) and not isinstance(obj, Symbol):
            obj = self.symbol(obj)
        sexpr = self.py_value_to_lisp_value([obj] + list(args))
        return self.lisp_value_to_py_value(self.eval(sexpr))

    def eval(self, sexpr, env=None):
        return leval(self, sexpr, self.globals if env is None else env)

    def execute(self, text):
        results = []

        def callback(sexpr):
            results.append(self.lisp_value_to_py_value(self.eval(sexpr)))

        self.parse(text, callback)
        return results

    def lookup(self, name, env=None):
        name = self.symbol(str(name))
        env = self.globals if env is None else env
        try:
            return env.find(name)[name]
        except NameError:
            return None

    main = main

    def parse(self, text, callback):
        p = Parser(self, callback)
        p.feed(text)
        p.feed(None)

    def repl(self, callback):
        return repl(self, callback)

    def type(self, x):
        return ltype(self, x)

    ########################################################################
    ## development type stuff

    spcl = spcl
    glbl = glbl
    ffi = ffi

    unary = unary
    binary = binary

    trampoline = trampoline
    bounce = bounce
    land = land

    SENTINEL = SENTINEL

    Continuation = Continuation
    Environment = Environment
    Lambda = Lambda
    ListBuilder = ListBuilder
    Struct = Struct
    Symbol = Symbol

    def fpop(self):
        if self.stack is EL:
            raise ValueError("stack empty")
        ret, self.stack = car(self, self.stack), cdr(self, self.stack)
        return ret

    def fpush(self, *frames, **kw):
        self.stack = cons(self, Struct(*frames, **kw), self.stack)

    def freset(self):
        self.stack = EL

    def frestore(self, state):
        self.stack = state

    def fsave(self):
        ## if we used py list/append/pop we'd have to slice an
        ## entire copy for a continuation. this way, we don't
        ## have to do anything at all.
        return self.stack

    def lisp_value_to_py_value(self, x):
        return lisp_value_to_py_value(self, x)

    def py_value_to_lisp_value(self, x):
        return py_value_to_lisp_value(self, x)

    def lisp_list_to_py_list(self, l):
        return lisp_list_to_py_list(self, l)

    def py_list_to_lisp_list(self, x):
        return py_list_to_lisp_list(self, x)

    def unpack(self, x, n):
        return unpack(self, x, n)


lisp = Lisp()


## }}}

if __name__ == "__main__":
    main()


## EOF
