#!/usr/bin/env python3

"lwp.py"

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring


SENTINEL = object()


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
## {{{ special forms


class SpecialForms(Operators):
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

    @spcl("lambda")
    def op_lambda(self, g, frame):
        params, body = g.unpack(frame.x, 2)

        if not (g.rpn.is_pair(params) or g.rpn.is_empty_list(params)):
            raise TypeError("expected param list, got {params!r}")

        return bounce(frame.c, g, Lambda(g, params, body, frame.e))

    @spcl("quote")
    def op_quote(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        return bounce(frame.c, g, x)


## }}}


def test():
    g = Globals(SpecialForms)
    assert g.rpn.eq(g.symbol("a"), g.symbol("ab"[0]))


if __name__ == "__main__":
    test()


## EOF
