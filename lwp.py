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
    def __init__(self):
        self.symtab = self.new_symbol_table()

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

    def symbol(self, string):
        return self.symtab.symbol(string)

    def is_symbol(self, x):
        return isinstance(x, self.Symbol)

    def is_atom(self, x):
        return self.is_empty_list(x) or self.is_symbol(x) or self.is_true(x)

    def eq(self, x, y):
        return self.is_atom(x) and x is y

    ## }}}
    ## {{{ pair, string, number

    ## pair and list

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

    def is_list(self, x):
        return self.is_pair(x) and self.is_pair(x.cdr())

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
    ## {{{ constructors

    def new_frame_stack(self):
        return FrameStack(self)

    def new_queue(self):
        return Queue(self)

    def new_stack(self):
        return Stack(self)

    def new_string_keyed_table(self):
        return StringKeyedTable(self)

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
## {{{ string keyed table


class StringKeyedTable:
    def __init__(self, rpn):
        self.rpn = rpn
        self.t = rpn.EL

    def get_data_structure(self):
        return self.t

    def set_data_structure(self, t):
        self.t = t

    def find(self, key):
        if not self.rpn.is_string(key):
            raise TypeError(f"expected string, got {key!r}")

        prev = self.rpn.EL
        node = self.t

        while not self.rpn.is_empty_list(node):
            pair = self.rpn.car(node)
            if self.rpn.string_equal(key, self.rpn.car(pair)):
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


class SymbolTable(StringKeyedTable):
    def symbol(self, string):
        ## this should only be called with string literals and parsed strings
        if type(string) is not str:  ## pylint: disable=unidiomatic-type-check
            raise TypeError(f"expected string, got {string!r}")
        return self.t.setdefault(string, self.rpn.Symbol(string))


## }}}


## }}}
