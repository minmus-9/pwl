#!/usr/bin/env python3

"lwp.py"

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring

import threading


LOCK = threading.Lock()
SENTINEL = object()


## {{{ stack frame


class Frame:
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
## {{{ atoms


class EL_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "()"


EL = EL_()
del EL_


def is_empty_list(x):
    return x is EL


class T_:
    def __repr__(self):
        return "#t"


T = T_()
del T_


def is_true(x):
    return x is T


class Symbol(str):
    ...


def is_symbol(x):
    return isinstance(x, Symbol)


def is_integer(x):
    return isinstance(x, int)


def is_float(x):
    return isinstance(x, float)


def is_number(x):
    return is_integer(x) or is_float(x)


def is_atom(x):
    return (
        is_empty_list(x)
        or is_symbol(x)
        or is_true(x)
    )


def eq(x, y):
    return is_atom(x) and x is y


## }}}
## {{{ compound types


def is_string(x):
    return isinstance(x, str) and not is_symbol(x)


## }}}
