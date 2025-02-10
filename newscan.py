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

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring


## {{{ basics


EL = object()


def car(x):
    return x[0]


def cdr(x):
    return x if x is EL else x[1]


def cons(x, y):
    return [x, y]


def set_car(x, y):
    x[0] = y


def set_cdr(x, y):
    x[1] = y


def splitcar(x):
    return x[0], x[1]


## }}}
## {{{ symbol


class Symbol:
    __slots__ = ["s"]

    def __init__(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        self.s = s

    def __repr__(self):
        return self.s


SYMBOLS = {}


def symbol(s):
    if s not in SYMBOLS:
        SYMBOLS[s] = Symbol(s)
    return SYMBOLS[s]


## }}}
## {{{ queue


class Queue:
    __slots__ = ["h", "t"]

    def __init__(self):
        self.h = self.t = EL

    def enqueue(self, x):
        n = cons(x, EL)
        if self.h is EL:
            self.h = n
        else:
            set_cdr(self.t, n)
        self.t = n

    def dequeue(self):
        n = self.h
        self.h = cdr(n)
        if self.h is EL:
            self.t = EL
        if n is EL:
            raise ValueError("queue is empty")
        return car(n)

    def head(self):
        return self.h


## }}}
## {{{ scanner


class Scanner:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.lut = {
            "(": self.c_lpar,
            ")": self.c_rpar,
            " ": self.c_ws,
            "\n": self.c_ws,
            "\r": self.c_ws,
            "\t": self.c_ws,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_semi,
            "'": self.c_tick,
            '"': self.c_quote,
            ",": self.c_comma,
            "`": self.c_backtick,
        }
        self.parens = []
        self.cont = self.k_sym
        self.callback = callback
        ## XXX ascii-specific
        for i in range(256):
            self.lut.setdefault(chr(i), self.token.append)

    def feed(self, text):
        if text is None:
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
        else:
            self.pos, n = 0, len(text)
            cont = self.cont
            while self.pos < n:
                p = self.pos
                ch = text[p]
                self.pos = p + 1
                cont = cont(ch) or cont
            self.cont = cont

    def push(self, ttype):
        l = self.token
        if l:
            t = "".join(l)
            l.clear()
        elif ttype == self.T_SYM:
            return
        else:
            self.callback(ttype, None)
            return
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

    def k_sym(self, ch):
        return self.lut[ch](ch)

    def k_comment(self, ch):
        return self.k_sym if ch in "\n\r" else self.k_comment

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == "\"":
            self.push(self.T_STRING)
            return self.k_sym
        self.token.append(ch)
        return self.k_quote

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def k_backslash(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.token.append(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.token.append("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_quote(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        return self.k_quote

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(",")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(ch)
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.append(ch)
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rpar(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '['")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rbrack(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '('")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}
## {{{ parser


class Parser:
    Q_MAP = {
        "'": symbol("quote"),
        "`": symbol("quasiquote"),
        ",": symbol("unquote"),
        ",@": symbol("unquote-splicing"),
    }

    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.lut = {
            self.scanner.T_SYM: self.t_sym,
            self.scanner.T_INT: self.add,
            self.scanner.T_FLOAT: self.add,
            self.scanner.T_STRING: self.add,
            self.scanner.T_TICK: self.set_up_quote,
            self.scanner.T_BACKTICK: self.set_up_quote,
            self.scanner.T_COMMA: self.set_up_quote,
            self.scanner.T_COMMA_AT: self.set_up_quote,
            self.scanner.T_LPAR: self.t_lpar,
            self.scanner.T_RPAR: self.t_rpar,
            self.scanner.T_EOF: self.t_eof,
        }

    def t_sym(self, token):
        self.add(symbol(token))

    def t_lpar(self, _):
        self.qstack.append(")")
        self.stack.append(Queue())

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
        self.lut[ttype](token)

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        ret = x
        while self.qstack and isinstance(self.qstack[-1], Symbol):
            s = self.qstack.pop()
            ret = cons(s, cons(ret, EL))
        return ret

    def set_up_quote(self, s):
        s = self.Q_MAP[s]
        self.qstack.append(s)


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
## {{{ recursive stringify


def stringify(x):
    if x is EL:
        return "()"
    if isinstance(x, (int, float, str, Symbol)):
        return str(x)
    if not isinstance(x, list):
        return repr(x)
    y = []
    while x is not EL:
        z, x = splitcar(x)
        y.append(stringify(z))
    return "(" + " ".join(y) + ")"


## }}}


def main():
    import time  ## pylint: disable=import-outside-toplevel

    s = Scanner(lambda *_: None)
    it = s
    p = Parser(lambda expr: print(stringify(expr)))
    p = Parser(lambda _: None)
    it = p
    n = 1
    n = 10000
    src = """
        (add 1 2.1); comment
        (error "abc")
        (let ([x 1] [y '2]) `,(add x y (,z) ,@()))
        (define !1 (lambda (n)
            (if
                (define n! 1)
                ()
                ((lambda (c _ _)                ;; huh. gotta love it!
                    (if (lt? n 2) n! (c c)))    ;; misleading formatting++
                    (call/cc (lambda (cc) cc))
                    (set! n! (mul n! n))
                    (set! n (sub n 1))
                )
            )
        ))
    """
    b = n * len(src)
    t0 = time.time()
    for _ in range(n):
        it.feed(src)
    it.feed(None)
    dt = time.time() - t0
    print(dt, b / dt)


if __name__ == "__main__":
    main()
