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

"parsers.py"

## pylint: disable=invalid-name
## XXX pylint: disable-missing-docstring

from scanners import Scanner5 as Scanner
from scanners import RUNTIME

## {{{ atoms


EL = object()
T = True


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    __repr__ = __str__


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y and is_atom(x)


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ symbol table


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("t",)

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        if s in self.t:
            return self.t[s]
        self.t[s] = ret = Symbol(s)
        return ret


symbol = SymbolTable().symbol


## }}}
## {{{ pairs


def cons(x, y):
    return [x, y]


## }}}
## {{{ stack


class Stack:
    __slots__ = ("s",)

    def __init__(self):
        self.s = EL

    def __bool__(self):
        return self.s is not EL

    def pop(self):
        ret, self.s = self.s
        return ret

    def push(self, x):
        self.s = [x, self.s]

    def top(self):
        return self.s[0]


## }}}
## {{{ queue


class Queue:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def dequeue(self):
        n = self.h
        self.h = n[1]
        if self.h is EL:
            self.t = EL
        return n[0]

    def enqueue(self, x):
        n = [x, EL]
        if self.h is EL:
            self.h = n
        else:
            self.t[1] = n
        self.t = n

    def head(self):
        return self.h


## }}}
## {{{ parser #1 reference


class Parser1:
    def __init__(self, callback):
        self.callback = callback
        self.stack = Stack()
        self.qstack = Stack()
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def t_sym(self, token):
        self.add(symbol(token))

    def t_lpar(self, _):
        self.qstack.push(")")
        self.stack.push(Queue())

    def t_rpar(self, _):
        assert self.stack  ## Scanner checks this
        assert self.qstack.pop() == ")"
        l = self.quote_wrap(self.stack.pop().head())
        if not self.stack:
            self.callback(l)
        else:
            self.add(l)

    def t_eof(self, _):
        assert not self.stack  ## Scanner checks this
        if self.qstack:
            raise SyntaxError("unclosed quasiquote")

    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.t_sym(token)
        elif ttype == s.T_LPAR:
            self.t_lpar(token)
        elif ttype == s.T_RPAR:
            self.t_rpar(token)
        elif ttype in (s.T_INT, s.T_FLOAT, s.T_STRING):
            self.add(token)
        elif ttype in (s.T_TICK, s.T_COMMA, s.T_COMMA_AT, s.T_BACKTICK):
            self.set_up_quote(token)
        else:
            self.t_eof(token)

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack.top().enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        ret = x
        while self.qstack and isinstance(self.qstack.top(), Symbol):
            s = self.qstack.pop()
            ret = cons(s, cons(ret, EL))
        return ret

    def set_up_quote(self, s):
        if s == "'":
            s = symbol("quote")
        elif s == ",":
            s = symbol("unquote")
        elif s == ",@":
            s = symbol("unquote-splicing")
        else:
            assert s == "`"
            s = symbol("quasiquote")
        self.qstack.push(s)


## }}}
## {{{ parser #2 inlined


class Parser2:
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
        elif ttype == s.T_INT or ttype == s.T_FLOAT or ttype == s.T_STRING:
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


def main():
    import time  ## pylint: disable=import-outside-toplevel

    K = Parser1  ## 2.01
    K = Parser2  ## 1.66
    f = lambda *_: None
    n = 200
    s = RUNTIME
    b = n * len(s)
    t = time.time()
    S = K(f)
    for _ in range(n):
        S.feed(s)
    S.feed(None)
    dt = time.time() - t
    print(b, dt, 1e6 * dt / b, b / dt)


if __name__ == "__main__":
    main()


## }}}
