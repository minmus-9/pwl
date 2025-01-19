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
pylisp.py - lisp.py with a python runtime added

this stuff doesn't do any type checking so not bullet-proof
"""

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import math
import random
import time


__all__ = ("lisp",)


from lisp import lisp


UNSPECIFIED = object()


@lisp.ffi(">float")
def ffi_to_float(x):
    return float(x)


@lisp.ffi(">int")
def ffi_to_int(x, base=10):
    return int(x, base)


@lisp.ffi(">string")
def ffi_to_str(x):
    return str(x)


@lisp.ffi(">symbol")
def ffi_to_sym(x):
    return x if isinstance(x, lisp.Symbol) else lisp.symbol(x)


@lisp.ffi("chr")
def ffi_chr(x):
    return chr(x)


@lisp.ffi("list")
def ffi_list(lst, attr, *args):
    ret = getattr(lst, str(attr))(*args)
    return lst if ret is None else ret


@lisp.ffi("list.append")
def ffi_list_append(l, x):
    l.append(x)
    return l


@lisp.ffi("list.concat")
def ffi_list_concat(*lists):
    ret = []
    for l in lists:
        ret.extend(l)
    return ret


@lisp.ffi("list.index")
def ffi_list_index(l, x):
    try:
        return l.index(x)
    except ValueError:
        return None


@lisp.ffi("list.length")
def ffi_list_length(l):
    return 0 if l is None else len(l)


@lisp.ffi("list.mul")
def ffi_list_mul(l, n):
    return l * n


@lisp.ffi("list.pop")
def ffi_list_pop(l):
    x = l.pop()
    return [x, l]


@lisp.ffi("list.reverse")
def ffi_list_reverse(lst):
    return list(reversed(lst))


@lisp.ffi("list.sort")
def ffi_list_sort(l):
    l.sort()
    return l


@lisp.ffi("list.splice")
def ffi_list_splice(l, bgn, end, x):
    if bgn is None:
        if end is None:
            l[:] = x
        else:
            l[:end] = x
    elif end is None:
        l[bgn:] = x
    else:
        l[bgn:end] = x


@lisp.ffi("list.sublist")
def ffi_list_sublist(l, bgn, end):
    if l is None:
        l = []
    if bgn is None:
        if end is None:
            return l[:]
        return l[:end]
    if end is None:
        return l[bgn:]
    return l[bgn:end]


@lisp.ffi("math")
def ffi_math(func, *args):
    obj = getattr(math, func)
    if callable(obj):
        return obj(*args)
    if args:
        raise TypeError(f"{func} takes no args")
    return obj


@lisp.ffi("ord")
def ffi_ord(ch):
    return ord(ch)


@lisp.ffi("pow")
def ffi_pow(x, n, m=None):
    if m is None:
        return pow(x, n)
    return pow(x, n, m)


@lisp.ffi("random")
def ffi_random(*args):
    if args:
        attr, *args = args
    else:
        attr = "random"
    return getattr(random, attr)(*args)


@lisp.ffi("random.choices")
def ffi_random_choices(pop, k, wts, cum_wts):
    return random.choices(pop, wts, cum_weights=cum_wts, k=k)


@lisp.ffi("random.shuffle")
def ffi_random_shuffle(items):
    random.shuffle(items)
    return items


@lisp.ffi("range")
def ffi_range(stop, arg2=UNSPECIFIED, arg3=UNSPECIFIED):
    if arg2 is UNSPECIFIED:
        assert arg3 is UNSPECIFIED
        x = range(stop)
    else:
        start, stop = stop, arg2
        if arg3 is UNSPECIFIED:
            x = range(start, stop)
        else:
            x = range(start, stop, arg3)
    return list(x)


@lisp.ffi("round")
def ffi_round(x, n=None):
    if n is None:
        return round(x)
    return round(x, n)


@lisp.ffi("string")
def ffi_string(s, attr, *args):
    return getattr(s, attr)(*args)


@lisp.ffi("string.equal?")
def ffi_string_eq(s, t):
    return s == t


@lisp.ffi("string.length")
def ffi_string_length(s):
    return len(s)


@lisp.ffi("string.lt?")
def ffi_string_lt(s, t):
    return s < t


@lisp.ffi("string.mul")
def ffi_string_mul(s, n):
    return s * n


@lisp.ffi("string.substr")
def ffi_string_substr(s, bgn, end):
    if bgn is None:
        if end is None:
            return s[:]
        return s[:end]
    if end is None:
        return s[bgn:]
    return s[bgn:end]


@lisp.ffi("time")
def ffi_time(attr, *args):
    ## time brokers in time tuples a lot, so just convert lists to tuples
    args = [tuple(z) if isinstance(z, list) else z for z in args]
    return getattr(time, attr)(*args)


if __name__ == "__main__":
    lisp.main()

## EOF
