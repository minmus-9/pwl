#!/usr/bin/env python3

"lwp.py"

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring

import sys


SENTINEL = object()


## {{{ error


class LispError(Exception):
    ...


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
## {{{ object representation


class Representation:
    ## pylint: disable=too-many-public-methods

    ## {{{ atoms
    class EL_:
        ## pylint: disable=too-few-public-methods

        def __repr__(self):
            return "()"

    EL = EL_()  ## empty list
    del EL_

    def is_empty_list(self, x):
        return x is self.EL

    class T_:
        ## pylint: disable=too-few-public-methods

        def __repr__(self):
            return "#t"

    T = T_()  ## #t
    del T_

    def is_true(self, x):
        return x is self.T

    class Symbol(str):
        ...

    ## symbols are unique within the scope of a Representation instance

    def is_symbol(self, x):
        return isinstance(x, self.Symbol)

    def is_atom(self, x):
        return self.is_empty_list(x) or self.is_symbol(x) or self.is_true(x)

    def eq(self, x, y):
        return self.is_atom(x) and x is y

    ## }}}
    ## {{{ pair, string, number

    class Pair(list):
        def __init__(self, rpn, car, cdr):
            super().__init__()
            self.rpn = rpn
            self[:] = [car, cdr]

        def car(self):
            return self[0]

        def cdr(self):
            return self[1]

        def set_car(self, x):
            self[0] = x
            return self.rpn.EL

        def set_cdr(self, x):
            self[1] = x
            return self.rpn.EL

    def is_pair(self, x):
        return isinstance(x, self.Pair)

    def car(self, x):
        if not self.is_pair(x):
            raise TypeError(f"expected pair, got {x!r}")
        return x.car()

    def cdr(self, x):
        if not self.is_pair(x):
            raise TypeError(f"expected pair, got {x!r}")
        return x.cdr()

    def cons(self, x, y):
        return self.Pair(self, x, y)

    def set_car(self, pair, x):
        if not self.is_pair(pair):
            raise TypeError(f"expected pair, got {pair!r}")
        return pair.set_car(x)

    def set_cdr(self, pair, x):
        if not self.is_pair(pair):
            raise TypeError(f"expected pair, got {pair!r}")
        return pair.set_cdr(x)

    ## string

    def is_string(self, x):
        return isinstance(x, str) and not self.is_symbol(x)

    def string_equal(self, s1, s2):
        assert self.is_string(s1) and self.is_string(s2)
        return s1 == s2

    ### numbers

    def is_integer(self, x):
        ## pylint: disable=no-self-use
        return isinstance(x, int)

    def is_float(self, x):
        ## pylint: disable=no-self-use
        return isinstance(x, float)

    def is_number(self, x):
        return self.is_integer(x) or self.is_float(x)

    ## }}}
    ## {{{ factories

    def new_frame_stack(self):
        return FrameStack(self)

    def new_queue(self):
        return Queue(self)

    def new_stack(self):
        return Stack(self)

    def new_keyed_table(self, compare):
        return KeyedTable(self, compare)

    def new_symbol_table(self):
        return SymbolTable(self)


## }}}


## }}}
## {{{ stack class


class Stack:
    def __init__(self, rpn):
        self.rpn = rpn
        self.stack = rpn.EL

    def get_data_structure(self):
        return self.stack

    def set_data_structure(self, stack):
        self.stack = stack

    ###

    def pop(self):
        if self.rpn.is_empty_list(self.stack):
            raise ValueError("stack is empty")
        ret = self.rpn.car(self.stack)
        self.stack = self.rpn.cdr(self.stack)
        return ret

    def push(self, x):
        self.stack = self.rpn.cons(x, self.stack)

    def top(self):
        if self.rpn.is_empty_list(self.stack):
            raise ValueError("stack is empty")
        return self.rpn.car(self.stack)


## }}}
## {{{ stack frame


class Frame:
    ## pylint: disable=too-few-public-methods

    ## this is just a c struct and it doesn't use list primitives for attr storage

    def __init__(self, *frames, **kw):
        for frame in frames:
            if not isinstance(frame, Frame):
                raise TypeError(f"expected Frame, got {frame!r}")
            self.__dict__.update(frame.__dict__)
        self.__dict__.update(kw)


## }}}
## {{{ frame stack class


class FrameStack(Stack):
    def push(self, *frames, **kw):
        super().push(Frame(*frames, **kw))


## }}}
## {{{ queue


class Queue:
    def __init__(self, rpn):
        self.rpn = rpn
        self.ht = rpn.cons(rpn.EL, rpn.EL)

    def get_data_structure(self):
        return self.ht

    def set_data_structure(self, pair):
        self.ht = pair

    def get_queue(self):
        return self.head()

    ###

    def head(self, new=SENTINEL):
        if new is SENTINEL:
            return self.rpn.car(self.ht)
        return self.rpn.set_car(self.ht, new)

    def tail(self, new=SENTINEL):
        if new is SENTINEL:
            return self.rpn.cdr(self.ht)
        return self.rpn.set_cdr(self.ht, new)

    def dequeue(self):
        if self.rpn.is_empty_list(self.head()):
            raise ValueError("queue is empty")
        head = self.head()
        ret, head = self.rpn.car(head), self.rpn.cdr(head)
        self.head(head)
        if self.rpn.is_empty_list(head):
            self.tail(self.rpn.EL)
        return ret

    def is_empty(self):
        return self.rpn.is_empty_list(self.head())

    def enqueue(self, x):
        node = self.rpn.cons(x, self.rpn.EL)
        if self.rpn.is_empty_list(self.head()):
            self.head(node)
        else:
            self.rpn.set_cdr(self.tail(), node)
        self.tail(node)


## }}}
## {{{ keyed table


class KeyedTable:
    ## NB compare() has to do its own type-checking! this also means
    ##    that it needs to be bound to the same Representation!
    def __init__(self, rpn, compare):
        self.rpn = rpn
        self.cmp = compare
        self.t = rpn.EL

    def get_data_structure(self):
        return self.t

    def set_data_structure(self, t):
        self.t = t

    def find(self, key):
        prev = self.rpn.EL
        node = self.t

        while not self.rpn.is_empty_list(node):
            pair = self.rpn.car(node)
            if self.cmp(key, self.rpn.car(pair)):
                return prev, node
            prev = node
            node = self.rpn.cdr(node)

        return self.rpn.EL, self.rpn.EL

    ###

    def delete(self, key):
        prev, node = self.find(key)
        if self.rpn.is_empty_list(node):
            return self.rpn.EL
        if self.rpn.is_empty_list(prev):
            self.t = self.rpn.cdr(node)
        else:
            self.rpn.set_cdr(prev, self.rpn.cdr(node))
        return self.rpn.T

    def get(self, key, default=SENTINEL):
        _, node = self.find(key)
        if self.rpn.is_empty_list(node):
            return default
        pair = self.rpn.car(node)
        return self.rpn.cdr(pair)

    def set(self, key, value):
        _, node = self.find(key)
        if self.rpn.is_empty_list(node):
            node = self.rpn.cons(key, value)
            self.t = self.rpn.cons(node, self.t)
        else:
            pair = self.rpn.car(node)
            self.rpn.set_cdr(pair, value)
        return value

    def setdefault(self, key, value):
        _, node = self.find(key)
        if self.rpn.is_empty_list(node):
            pair = self.rpn.cons(key, value)
            self.t = self.rpn.cons(pair, self.t)
        else:
            pair = self.rpn.car(node)
            value = self.rpn.cdr(pair)
        return value


## }}}
## {{{ symbol table


class SymbolTable(KeyedTable):
    def __init__(self, rpn):
        super().__init__(rpn, rpn.string_equal)

    def symbol(self, string):
        ## this should only be called with string literals and parsed strings
        if type(string) is not str:  ## pylint: disable=unidiomatic-typecheck
            raise TypeError(f"expected string, got {string!r}")
        return self.setdefault(string, self.rpn.Symbol(string))


## }}}
## {{{ environment


class Environment:
    def __init__(self, g, params, args, parent):
        assert isinstance(parent, Environment) or g.rpn.is_empty_list(parent)
        self.g = g
        self.sp = g.rpn.cons(g.rpn.new_keyed_table(g.rpn.eq), parent)
        self.bind(params, args)

    def bind(self, params, args):
        rpn = self.g.rpn
        variadic = False
        while not rpn.is_empty_list(params):
            if not rpn.is_pair(params):
                raise RuntimeError("bad params")
            if not rpn.is_pair(args):
                raise RuntimeError("bad args")
            p, params = rpn.car(params), rpn.cdr(params)
            if not rpn.is_symbol(p):
                raise TypeError(f"expected symbol, got {p!r}")
            if rpn.eq(p, rpn.symbol("&")):
                variadic = True
            elif variadic:
                if not rpn.is_empty_list(params):
                    raise SyntaxError("too many params after '&'")
                self.set(p, args)
                return
            elif rpn.is_empty_list(args):
                raise SyntaxError("not enough args")
            else:
                self.set(p, rpn.car(args))
                args = rpn.cdr(args)
        if variadic:
            raise SyntaxError("args end with '&'")
        if not rpn.is_empty_list(args):
            raise SyntaxError("too many args")

    def stab(self):
        return self.g.rpn.car(self.sp)

    def parent(self):
        return self.g.rpn.cdr(self.sp)

    ###

    def delete(self, sym):
        return self.stab().delete(sym)

    def get(self, sym, default=SENTINEL):
        rpn = self.g.rpn
        e = self
        while not rpn.is_empty_list(e):
            x = e.stab().get(sym)
            if x is not SENTINEL:
                return x
            e = e.parent()
        return default

    def set(self, sym, value):
        return self.stab().set(sym, value)

    def setbang(self, sym, value):
        rpn = self.g.rpn
        e = self
        while not rpn.is_empty_list(e):
            t = self.stab()
            ## XXX could add a table method for this
            x = t.get(sym)
            if x is not SENTINEL:
                t.set(sym, value)
                break
            e = e.parent()
        return rpn.EL


## }}}
## {{{ globals class


class Globals:
    ## this is just a c struct and it doesn't use list primitives for attr storage

    REPRESENTATION_CLASS = Representation

    def __init__(
        self, operator_class, representation=None, representation_class=None
    ):
        self.rpn = (
            representation
            if representation
            else (representation_class or self.REPRESENTATION_CLASS)()
        )

        self.stab = self.rpn.new_symbol_table()
        self.stack = self.rpn.new_frame_stack()
        self.ops = operator_class(self)
        self.genv, self.senv = self.ops.get_envs()

    ## {{{ global symbol table

    def symbol(self, string):
        return self.stab.symbol(string)

    ## }}}
    ## {{{ argument unpacker

    def unpack(self, lst, n):
        rpn = self.rpn
        ret = []
        for _ in range(n):
            if rpn.is_empty_list(lst):
                raise TypeError(f"not enough args, expected {n}")
            if not rpn.is_pair(lst):
                ## so we know we're a list at each iteration
                raise RuntimeError("bad list passed to unpack()")
            ret.append(rpn.car(lst))
            lst = rpn.cdr(lst)
        if not rpn.is_empty_list(lst):
            raise TypeError(f"too many args, expected {n}")
        return ret

    ## }}}
    ## {{{ (partial) obj-to-type-symbol

    def type(self, x):
        ## pylint: disable=too-many-return-statements
        rpn = self.rpn
        if rpn.is_empty_list(x):
            return self.symbol("()")
        if rpn.is_true(x):
            return self.symbol("#t")
        if rpn.is_pair(x):
            return self.symbol("pair")
        if rpn.is_symbol(x):
            return self.symbol("symbol")
        if rpn.is_integer(x):
            return self.symbol("integer")
        if rpn.is_float(x):
            return self.symbol("float")
        if rpn.is_string(x):
            return self.symbol("string")
        return SENTINEL

    ## }}}
    ## {{{ decorators for dynamic addition of ops

    def glbl(self, name):
        return self.ops.glbl(name)

    def spcl(self, name):
        return self.ops.spcl(name)

    ## }}}
    ## {{{ stringify

    def stringify(self, sexpr, genv=SENTINEL, senv=SENTINEL):
        e = self.genv if genv is SENTINEL else genv
        s = self.senv if senv is SENTINEL else senv
        return trampoline(
            self.stringify_, self, Frame(x=sexpr, e=e, s=s, c=land)
        )

    def stringify_setup(self, g, frame, args):
        assert g is self
        if self.rpn.is_pair(args):
            arg, args = self.car(args), self.cdr(args)
        else:
            arg, args = args, self.rpn.EL
            self.stack.push(".")  ## XXX parser needs to allow this for input!
        self.stack.fpush(frame, x=args)
        return bounce(
            self.stringify_, self, Frame(frame, x=arg, c=self.stringify_cont)
        )

    def stringify_cont(self, g, value):
        assert g is self
        rpn = self.rpn
        frame = self.stack.fpop()
        args = frame.x

        if rpn.is_empty_list(args):
            parts = [value]
            while True:
                f = self.stack.fpop()
                if f.x is SENTINEL:
                    break
                parts.insert(0, f.x)
            return bounce(frame.c, self, "(" + " ".join(parts) + ")")

        self.stack.fpush(frame, x=value)
        return self.stringify_setup(g, frame, args)

    def stringify_(self, g, frame):
        ## pylint: disable=too-many-return-statements
        assert g is self
        rpn = g.rpn
        x = frame.x
        if x is rpn.T:
            return bounce(frame.c, g, "#t")
        if x is rpn.EL:
            return bounce(frame.c, g, "()")
        if rpn.is_symbol(x) or rpn.is_number(x):
            return bounce(frame.c, g, str(x))
        if rpn.is_string(x):
            return bounce(
                frame.c, g, '"' + repr(x)[1:-1].replace('"', '\\"') + '"'
            )
        if is_lambda(x):
            return bounce(x.stringify_, g, frame)
        if is_continuation(x):
            return bounce(frame.c, g, "[continuation]")
        if callable(x):
            return bounce(frame.c, g, "[primitive]")
        if not rpn.is_pair(x):
            return bounce(frame.c, g, "[opaque]")

        g.stack.fpush(frame, x=SENTINEL)

        return self.stringify_setup(g, frame, x)

    ## }}}
    ## {{{ eval

    def eval(self, sexpr, genv=SENTINEL, senv=SENTINEL):
        e = self.genv if genv is SENTINEL else genv
        s = self.senv if senv is SENTINEL else senv
        return trampoline(self.eval_, self, Frame(x=sexpr, e=e, s=s, c=land))

    def eval_setup(self, frame, args):
        rpn = self.rpn
        if rpn.is_pair(args):
            arg, args = rpn.car(args), rpn.cdr(args)
        else:
            arg, args = args, rpn.EL
        self.stack.fpush(frame, x=args)
        return bounce(self.eval_, self, Frame(frame, x=arg, c=self.eval_next_arg))

    def eval_next_arg(self, g, value):
        assert g is self
        rpn = self.rpn
        frame = self.stack.fpop()
        args = frame.x

        if rpn.is_empty_list(args):
            ret = rpn.cons(value, rpn.EL)
            while True:
                f = self.stack.fpop()
                if f.x is SENTINEL:
                    proc = f.proc
                    break
                ret = rpn.cons(f.x, ret)
            return bounce(proc, g, Frame(frame, x=ret))

        self.stack.fpush(frame, x=value)
        return self.eval_setup(frame, args)


    def eval_proc_done(self, g, proc):
        assert g is self
        frame = self.stack.pop()
        args = frame.x

        if not callable(proc):  ## python func Lambda Continuation
            raise TypeError(f"expected callable, got {proc!r}")

        if getattr(proc, "special", False):
            return bounce(proc, self, frame)
        if self.rpn.is_empty_list(args):
            return bounce(proc, self, frame)

        self.stack.fpush(frame, proc=proc, x=SENTINEL)

        return self.eval_setup(frame, args)


    def eval_(self, g, frame):
        assert g is self
        rpn = self.rpn
        x = frame.x
        if rpn.is_symbol(x):
            obj = self.genv.get(x)
            if obj is SENTINEL:
                raise NameError(x)
            return bounce(frame.c, self, obj)
        ## NB test is_atom *after* is_symbol
        if rpn.is_atom(x) or rpn.is_number(x) or rpn.is_string(x):
            return bounce(frame.c, self, x)
        if callable(x):
            ## primitive Lambda Continuation
            if is_lambda(x):
                sym, args = x, rpn.EL
            else:
                return bounce(frame.c, self, x)
        elif not rpn.is_pair(x):
            raise RuntimeError("unexpected object type {x!r}")
        else:
            sym, args = rpn.car(x), rpn.cdr(x)
        if rpn.is_symbol(sym):
            op = self.senv.get(sym)
            if op is not SENTINEL:
                return bounce(op, self, Frame(frame, x=args))
        elif callable(sym):
            ## primitive Lambda Continuation
            self.stack.fpush(frame, proc=sym, x=args)
            return self.eval_proc_done(self, sym)
        elif not rpn.is_pair(sym):
            raise TypeError("expected proc or list, got {sym!r}")

        self.stack.fpush(frame, x=args)
        return bounce(self.eval_, self, Frame(frame, x=sym, c=self.eval_proc_done))

    ## }}}


## }}}
## {{{ lambda


class Lambda:
    special = False

    def __init__(
        self, g, params, body, genv, senv
    ):  ## pylint: disable=too-many-arguments
        self.r = g.rpn
        cons = g.rpn.cons
        self.info = cons(params, cons(body, cons(genv, cons(senv, g.rpn.EL))))

    def params(self):
        return self.r.car(self.info)

    def body(self):
        return self.r.car(self.r.cdr(self.info))

    def genv(self):
        return self.r.car(self.r.cdr(self.r.cdr(self.info)))

    def senv(self):
        return self.r.car(self.r.cdr(self.r.cdr(self.r.cdr(self.info))))

    def set_special(self):
        self.special = True

    def __call__(self, g, frame):
        args = frame.x
        parent = (
            frame.e if self.special else self.genv()
        )  ## specials are weird
        e = Environment(g, self.params(), args, parent)
        s = Environment(g, g.rpn.EL, g.rpn.EL, frame.s)
        return bounce(g.eval_, g, Frame(frame, x=self.body(), e=e, s=s))

    ###

    def lambda_body_done(self, g, bodystr):
        ## pylint: disable=no-self-use
        frame = g.stack.fpop()
        paramstr = frame.x
        return bounce(frame.c, g, "(lambda " + paramstr + " " + bodystr + ")")

    def lambda_params_done(self, g, paramstr):
        ## pylint: disable=no-self-use
        frame = g.stack.fpop()
        body = frame.x
        g.state.fpush(frame, x=paramstr)
        return bounce(
            g.stringify_, g, Frame(frame, x=body, c=self.lambda_body_done)
        )

    def stringify_(self, g, frame):
        g.stack.fpush(frame, x=self.body())
        return bounce(
            g.stringify_,
            g,
            Frame(frame, x=self.params(), c=self.lambda_params_done),
        )


def is_lambda(x):
    return isinstance(x, Lambda)


## }}}
## {{{ continuations


class Continuation:
    ## pylint: disable=too-few-public-methods

    def __init__(self, g, continuation):
        self.continuation = continuation  ## a python func
        self.stack = g.stack.get_data_structure()

    def __call__(self, g, frame):
        (x,) = g.unpack(frame.x, 1)
        g.stack.set_data_structure(self.stack)
        return bounce(self.continuation, g, x)  ## that's it.


def is_continuation(x):
    return isinstance(x, Continuation)


## }}}
## {{{ operator support


def glbl(name):
    def wrap(func):
        setattr(func, "lisp_op_type", ("glbl", name))
        return func

    return wrap


def spcl(name):
    def wrap(func):
        setattr(func, "lisp_op_type", ("spcl", name))
        return func

    return wrap


class Operators:
    ## this is a c struct

    ## pylint bait
    GLOBAL_ATTRS_ = SPECIAL_ATTRS_ = {}

    def __init__(self, g):
        self.g = g
        self.GLOBALS = Environment(g, g.rpn.EL, g.rpn.EL, g.rpn.EL)
        self.SPECIALS = Environment(g, g.rpn.EL, g.rpn.EL, g.rpn.EL)
        for k in self.GLOBAL_ATTRS_:
            value = getattr(self, k, None)
            if callable(value) and hasattr(value, "lisp_op_type"):
                sym = g.symbol(value.lisp_op_type[1])
                self.GLOBALS.set(sym, value)
        for k in self.SPECIAL_ATTRS_:
            value = getattr(self, k, None)
            if callable(value) and hasattr(value, "lisp_op_type"):
                sym = g.symbol(value.lisp_op_type[1])
                self.SPECIALS.set(sym, value)

    @classmethod
    def __init_subclass__(cls, **kw):
        super().__init_subclass__(**kw)
        glbls = {}
        spcls = {}
        lut = {"glbl": glbls, "spcl": spcls}
        for attr in dir(cls):
            value = getattr(cls, attr)
            tag = getattr(value, "lisp_op_type", None)
            if tag is not None:
                lut[tag[0]].setdefault(attr)
        cls.GLOBAL_ATTRS_ = glbls
        cls.SPECIAL_ATTRS_ = spcls

    def get_envs(self):
        return self.GLOBALS, self.SPECIALS

    ### dynamic op addition

    def glbl(self, name):
        def wrap(func):
            self.GLOBALS.set(self.g.symbol(name), func)
            return func

        return wrap

    def spcl(self, name):
        def wrap(func):
            self.SPECIALS.set(self.g.symbol(name), func)
            return func

        return wrap


## }}}
## {{{ base operators


class BaseOperators(Operators):
    ## pylint: disable=too-many-public-methods

    ## {{{ special forms

    def op_define_cont(self, g, value):
        ## pylint: disable=no-self-use
        frame = g.stack.pop()
        sym = frame.x
        frame.e.set(sym, value)
        return bounce(frame.c, g, g.rpn.EL)

    @spcl("define")
    def op_define(self, g, frame):
        sym, defn = g.unpack(frame.x, 2)

        if not g.rpn.is_symbol(sym):
            raise TypeError(f"expected symbol, got {sym!r}")

        g.stack.fpush(frame, x=sym)
        return bounce(g.eval_, g, Frame(frame, x=defn, c=self.op_define_cont))

    ###

    def op_if_cont(self, g, value):
        ## pylint: disable=no-self-use
        frame = g.stack.pop()
        ca = frame.x
        sexpr = g.rpn.cdr(ca) if g.rpn.is_empty_list(value) else g.rpn.car(ca)
        return bounce(g.eval_, g, Frame(frame, x=sexpr))

    @spcl("if")
    def op_if(self, g, frame):
        p, c, a = g.unpack(frame.x, 3)
        g.stack.fpush(frame, x=g.rpn.cons(c, a))
        return bounce(g.eval_, g, Frame(frame, x=p, c=self.op_if_cont))

    ###

    @spcl("lambda")
    def op_lambda(self, g, frame):
        ## pylint: disable=no-self-use
        params, body = g.unpack(frame.x, 2)

        if not (g.rpn.is_pair(params) or g.rpn.is_empty_list(params)):
            raise TypeError("expected param list, got {params!r}")

        return bounce(frame.c, g, Lambda(g, params, body, frame.e, frame.s))

    ## this follows https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html
    ## (special) doesn't quite get the job done due to the way its env works.
    ## it ain't the same as a recursive scheme macro :-) this quasiquote impl
    ## is longer than the rest of the special forms combined (!), but it
    ## helps the bootstrap a lot.

    def qq_list_setup(self, g, frame, form):
        rpn = g.rpn
        elt, form = rpn.car(form), rpn.cdr(form)
        if not (rpn.is_pair(form) or rpn.is_empty_list(form)):
            raise TypeError(f"expected list, got {form!r}")
        g.stack.fpush(frame, x=form)
        return bounce(
            self.qq_list_next, g, Frame(frame, x=elt, c=self.qq_list_cont)
        )

    def qq_finish(self, g, frame, value):
        ## pylint: disable=no-self-use
        rpn = g.rpn
        res = rpn.EL if value is SENTINEL else rpn.cons(value, rpn.EL)
        while True:
            f = g.stack.fpop()
            if f.x is SENTINEL:
                break
            res = rpn.cons(f.x, res)
        return bounce(frame.c, g, res)

    def qq_list_cont(self, g, value):
        frame = g.stack.fpop()
        form = frame.x

        if g.rpn.is_empty_list(form):
            return bounce(self.qq_finish, frame, value)

        g.stack.fpush(frame, x=value)

        return self.qq_list_setup(g, frame, form)

    def qq_spliced(self, g, value):
        frame = g.stack.fpop()
        form = frame.x
        rpn = g.rpn

        if rpn.is_empty_list(value):
            if rpn.is_empty_list(form):
                return bounce(self.qq_finish, g, frame, SENTINEL)
            return self.qq_list_setup(g, frame, form)

        while not rpn.is_empty_list(value):
            if not rpn.is_pair(value):
                raise TypeError(f"expected list, got {value!r}")
            elt, value = rpn.car(value), rpn.cdr(value)
            if rpn.is_empty_list(value):
                g.stack.fpush(frame, x=form)
                return bounce(self.qq_list_cont, g, elt)
            g.stack.fpush(frame, x=elt)

        raise RuntimeError("logs in the bedpan")

    def qq_list_next(self, g, frame):
        elt = frame.x
        rpn = g.rpn

        if rpn.is_pair(elt) and rpn.eq(
            rpn.car(elt), g.symbol("unquote-splicing")
        ):
            _, x = g.unpack(elt, 2)
            return bounce(g.eval_, g, Frame(frame, x=x, c=self.qq_spliced))
        return bounce(self.qq, g, Frame(frame, x=elt, c=self.qq_list_cont))

    def qq_list(self, g, frame):
        rpn = g.rpn
        form = frame.x
        app = rpn.car(form)

        if app is g.symbol("quasiquote"):
            _, x = g.unpack(form, 2)
            return bounce(self.qq, g, Frame(frame, x=x))

        if app is g.symbol("unquote"):
            _, x = g.unpack(form, 2)
            return bounce(g.eval_, g, Frame(frame, x=x))

        if app is g.symbol("unquote-splicing"):
            _, x = g.unpack(form, 2)
            raise LispError("cannot use unquote-splicing here")

        g.stack.fpush(frame, x=SENTINEL)

        return self.qq_list_setup(g, frame, form)

    def qq(self, g, frame):
        form = frame.x
        if g.rpn.is_pair(form):
            return bounce(self.qq_list, g, frame)
        return bounce(frame.c, g, form)

    @spcl("quasiquote")
    def op_quasiquote(self, g, frame):
        (form,) = g.unpack(frame.x, 1)
        return bounce(self.qq, g, Frame(frame, x=form))

    ###

    @spcl("quote")
    def op_quote(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        return bounce(frame.c, g, x)

    ###

    def op_setbang_cont(self, g, defn):
        ## pylint: disable=no-self-use
        frame = g.stack.fpop()
        sym = frame.x
        frame.e.setbang(sym, defn)
        return bounce(frame.c, g, g.rpn.EL)

    @spcl("set!")
    def op_setbang(self, g, frame):
        sym, defn = g.unpack(frame.x, 2)
        if not g.rpn.is_symbol(sym):
            raise TypeError(f"expected symbol, got {sym!r}")
        g.stack.fpush(frame, x=sym)
        return bounce(
            g.eval_, g, Frame(frame, x=defn, c=self.op_setbang_cont)
        )

    ###

    def op_special_cont(self, g, value):
        ## pylint: disable=no-self-use
        frame = g.stack.pop()
        sym = frame.x
        frame.s.set(sym, value)
        return bounce(frame.c, g, g.rpn.EL)

    @spcl("special")
    def op_special(self, g, frame):
        sym, defn = g.unpack(frame.x, 2)

        if not g.rpn.is_symbol(sym):
            raise TypeError(f"expected symbol, got {sym!r}")

        g.stack.fpush(frame, x=sym)
        return bounce(
            g.eval_, g, Frame(frame, x=defn, c=self.op_special_cont)
        )

    ###

    @spcl("trap")
    def op_trap(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        ok = g.rpn.T
        try:
            ## this has to be recursive because you can't pass
            ## exceptions across the trampoline. there is a chance
            ## of blowing the python stack here if you do a deeply
            ## recursive trap.
            res = g.eval(x, frame.e, frame.s)
        except:  ## pylint: disable=bare-except
            ok = g.rpn.EL
            t, v = sys.exc_info()[:2]
            res = f"{t.__name__}: {str(v)}"
        return bounce(frame.c, g, g.rpn.cons(ok, g.rpn.cons(res, g.rpn.EL)))

    ## }}}
    ## {{{ operators

    def unary(self, g, frame, func):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        return bounce(frame.c, g, func(x))

    def binary(self, g, frame, func):
        ## pylint: disable=no-self-use
        x, y = g.unpack(frame.x, 2)
        return bounce(frame.c, g, func(x, y))

    @glbl("atom?")
    def op_atom(self, g, frame):
        rpn = g.rpn

        def f(x):
            return rpn.T if rpn.is_atom(x) else rpn.EL

        return self.unary(g, frame, f)

    @glbl("call/cc")
    @glbl("call-with-current-continuation")
    def op_callcc(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        if not callable(x):
            raise TypeError(f"expected callable, got {x!r}")
        cc = Continuation(g, frame.c)
        arg = g.rpn.cons(cc, g.rpn.EL)
        return bounce(x, g, Frame(frame, x=arg))

    @glbl("car")
    def op_car(self, g, frame):
        return self.unary(g, frame, g.rpn.car)

    @glbl("cdr")
    def op_cdr(self, g, frame):
        return self.unary(g, frame, g.rpn.cdr)

    @glbl("cons")
    def op_cons(self, g, frame):
        return self.binary(g, frame, g.rpn.cons)

    @glbl("div")
    def op_div(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            ## XXX implicitly assuming integer == int!
            if rpn.is_integer(x) and rpn.is_integer(y):
                return x // y
            return x / y

        return self.binary(g, frame, f)

    @glbl("eq?")
    def op_eq(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            return rpn.T if rpn.eq(x, y) else rpn.EL

        return self.binary(g, frame, f)

    @glbl("equal?")
    def op_equal(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_number(x) and rpn.is_number(y)):
                raise TypeError(f"expected numbers, got {x!r} {y!r}")

            return rpn.T if x == y else rpn.EL

        return self.binary(g, frame, f)

    @glbl("error")
    def op_error(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        raise LispError(x)

    @glbl("eval")
    def op_eval(self, g, frame):
        ## pylint: disable=no-self-use
        rpn = g.rpn

        try:
            (x,) = g.unpack(frame.x, 1)
            n_up = 0
        except TypeError:
            x, n_up = g.unpack(frame.x, 2)

        if rpn.is_string(x):
            l = []
            p = Parser(g, l.append)
            p.feed(str(x))  ## XXX implicit conversion from lisp -> str
            p.feed(None)
            x = l[-1] if l else rpn.EL
        e = frame.e
        s = frame.s
        for _ in range(n_up):
            e = e.parent()
            s = s.parent()
            if rpn.is_empty_list(e) or rpn.is_empty_list(s):
                raise ValueError(f"cannot go up {n_up} levels")
        return bounce(g.eval_, g, Frame(frame, x=x, e=e, s=s))

    ###

    def op_exit_cont(self, g, value):
        ## pylint: disable=no-self-use
        raise SystemExit(value)

    @glbl("exit")
    def op_exit(self, g, frame):
        (x,) = g.unpack(frame.x, 1)
        if g.rpn.is_integer(x):
            raise SystemExit(x)
        return bounce(g.stringify_, g, Frame(frame, x=x, c=self.op_exit_cont))

    ###

    @glbl("lt?")
    def op_lt(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_number(x) and rpn.is_number(y)):
                raise TypeError(f"expected numbers, got {x!r} and {y!r}")
            return rpn.T if x < y else rpn.EL

        return self.binary(g, frame, f)

    @glbl("mul2")
    def op_mul2(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_number(x) and rpn.is_number(y)):
                raise TypeError(f"expected numbers, got {x!r} and {y!r}")
            return x * y  ## XXX implicit conversion from python to lisp

        return self.binary(g, frame, f)

    @glbl("nand")
    def op_nand(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_integer(x) and rpn.is_integer(y)):
                raise TypeError(f"expected integers, got {x!r} and {y!r}")
            return ~(x & y)  ## XXX implicit conversion

        return self.binary(g, frame, f)

    ###

    def op_print_cont(self, g, value):
        rpn = g.rpn
        frame = g.stack.pop()
        args = frame.x

        if rpn.is_empty_list(args):
            print(value)
            return bounce(frame.c, g, rpn.EL)
        print(value, end=" ")

        arg, args = rpn.car(args), rpn.cdr(args)

        g.stack.fpush(frame, x=args)
        return bounce(
            g.stringify_, g, Frame(frame, x=arg, c=self.op_print_cont)
        )

    @glbl("print")
    def op_print(self, g, frame):
        rpn = g.rpn
        args = frame.x

        ## NB we know args is a well-formed list because eval() created it

        if rpn.is_empty_list(args):
            print()
            return bounce(frame.c, g, rpn.EL)

        arg, args = rpn.car(args), rpn.cdr(args)

        g.stack.fpush(frame, x=args)
        return bounce(
            g.stringify_, g, Frame(frame, x=arg, c=self.op_print_cont)
        )

    ###

    @glbl("set-car!")
    def op_setcarbang(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            return rpn.setcarbang(x, y)

        return self.binary(g, frame, f)

    @glbl("set-cdr!")
    def op_setcdrbang(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            return rpn.setcdrbang(x, y)

        return self.binary(g, frame, f)

    @glbl("sub")
    def op_sub(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_number(x) and rpn.is_number(y)):
                raise TypeError(f"expected numbers, got {x!r} and {y!r}")
            return x - y  ## XXX implicit conversion from python to lisp

        return self.binary(g, frame, f)

    ## XXX for type converison >symbol etc we need to extend Representation

    @glbl("type")
    def op_type(self, g, frame):
        def f(x):
            t = g.type(x)
            if t is not SENTINEL:
                return t
            if isinstance(x, Lambda):
                return g.symbol("lambda")
            if isinstance(x, Continuation):
                return g.symbol("continuation")
            if callable(x):
                return g.symbol("primitive")
            return g.symbol("opaque")

        return self.unary(g, frame, f)

    ## }}}


## }}}
## {{{ lisp class


class Lisp:
    ...


## }}}


def test():
    g = Globals(BaseOperators)
    assert g.rpn.eq(g.symbol("a"), g.symbol("ab"[0]))


if __name__ == "__main__":
    test()


## EOF
