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


class LispError(Exception):
    pass


error = LispError


SENTINEL = object()


EL = object()
T = True


## }}}
## {{{ pair


class Pair:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["x", "y"]

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"{self.__class__.__name__}({self.x}, {self.y})"

def car(x):
    return x.x


def cdr(x):
    return EL if x is EL else x.y


def cons(x, y):
    return Pair(x, y)


def set_car(x, y):
    x.x = y


def set_cdr(x, y):
    x.y = y


def splitcar(x):
    return x.x, x.y


## }}}
## {{{ table


class Table:
    __slots__ = ["t", "c"]

    def __init__(self, compare):
        self.c = compare
        self.t = cons(EL, EL)

    def find(self, key):
        link = car(self.t)
        while link is not EL:
            node = car(link)
            if self.c(key, car(node)):
                return node
            link = cdr(link)
        return SENTINEL

    def get(self, key, default):
        node = self.find(key)
        if node is SENTINEL:
            return default
        return cdr(node)

    def set(self, key, value):
        node = self.find(key)
        if node is SENTINEL:
            node = cons(key, value)
            link = cons(node, car(self.t))
            set_car(self.t, link)
        else:
            set_cdr(node, value)


## }}}
## {{{ atoms and global symbol table


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["s"]

    def __init__(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        self.s = s

    def __repr__(self):
        return self.s


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return is_atom(x) and x is y


def string_cmp(x, y):
    ## pylint: disable=unidiomatic-typecheck
    return type(x) is str and type(y) is str and x == y


def symbol_cmp(x, y):
    return isinstance(x, Symbol) and x is y


SYMBOLS = Table(string_cmp)


def symbol(s):
    assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
    ret = SYMBOLS.get(s, SENTINEL)
    if ret is SENTINEL:
        ret = Symbol(s)
        SYMBOLS.set(s, ret)
    return ret


## }}}
## {{{ stack


class Stack:
    __slots__ = ["s"]

    def __init__(self):
        self.s = cons(EL, EL)

    def __bool__(self):
        return car(self.s) is not EL

    def clear(self):
        set_car(self.s, EL)

    def push(self, thing):
        set_car(self.s, cons(thing, car(self.s)))

    append = push

    def pop(self):
        top = car(self.s)
        if top is EL:
            raise ValueError("stack is empty")
        ret = car(top)
        set_car(self.s, cdr(top))
        return ret

    def top(self):
        top = car(self.s)
        if top is EL:
            raise ValueError("stack is empty")
        return car(top)

    ## for continuations

    def get(self):
        return self.s

    def set(self, value):
        self.s = value


## }}}
## {{{ stack frame


class Frame:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["x", "c", "e"]

    def __init__(self, f, x=None, c=None, e=None):
        assert isinstance(f, Frame) or f is SENTINEL
        self.x = f.x if x is None else x
        self.c = f.c if c is None else c
        self.e = f.e if e is None else e

    def __repr__(self):
        return f"{self.__class__.__name__}({self.x}, {self.c}, {self.e})"


## }}}
## {{{ frame stack and global stack


class FrameStack(Stack):
    def push(self, thing, **kw):
        super().push(Frame(thing, **kw))


stack = FrameStack()


## }}}
## {{{ queue


class Queue:
    __slots__ = ["n"]

    def __init__(self):
        self.n = cons(EL, EL)

    def __bool__(self):
        return car(self.n) is not EL

    def head(self):
        return car(self.n)

    def enqueue(self, x):
        node = cons(x, EL)
        if car(self.n) is EL:
            set_car(self.n, node)
        else:
            set_cdr(cdr(self.n), node)
        set_cdr(self.n, node)

    append = enqueue

    def dequeue(self):
        node = car(self.n)
        if node is EL:
            raise ValueError("queue is empty")
        h = cdr(node)
        set_car(self.n, cdr(node))
        if h is EL:
            set_cdr(self.n, EL)
        return car(node)


## }}}
## {{{ environment and global genv


class Environment:
    __slots__ = ["e"]

    def __init__(self, params, args, parent):
        self.e = cons(Table(symbol_cmp), parent)
        self.bind(params, args)

    def bind(self, params, args):
        pl = params
        al = args
        t = car(self.e)
        variadic = False
        while params is not EL:
            p, params = splitcar(params)
            if not isinstance(p, Symbol):
                raise TypeError(
                    f"expected symbol, got {p!r} at {pl!r} <= {al!r}"
                )
            if eq(p, symbol("&")):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError(f"extra junk {params!r} after '&'")
                t.set(p, args)
                return
            elif args is EL:
                raise TypeError(f"not enough args at {pl!r} <= {al!r}")
            else:
                a, args = splitcar(args)
                t.set(p, a)
        if variadic:
            raise SyntaxError(f"'&' ends param list {pl!r} <= {al!r}")
        if args is not EL:
            raise TypeError(f"too many args at {pl!r} <= {al!r}")

    def get(self, sym, default):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            t = car(e.e)
            x = t.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = cdr(e.e)
        return default

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        car(self.e).set(sym, value)
        return EL

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            t = car(e.e)
            if t.get(sym, SENTINEL) is not SENTINEL:
                t.set(sym, value)
                return EL
            e = cdr(e.e)
        raise NameError(str(sym))


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


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
        self.token = Queue()
        self.parens = Stack()
        self.cont = self.k_sym
        self.callback = callback

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
            t = ""
            while l:
                t += l.dequeue()
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
        if ch == "(":
            return self.c_lpar(ch)
        if ch == ")":
            return self.c_rpar(ch)
        if ch in " \n\r\t":
            return self.c_ws(ch)
        if ch == "[":
            return self.c_lbrack(ch)
        if ch == "]":
            return self.c_rbrack(ch)
        if ch == ";":
            return self.c_semi(ch)
        if ch == "'":
            return self.c_tick(ch)
        if ch == '"':
            return self.c_quote(ch)
        if ch == ",":
            return self.c_comma(ch)
        if ch == "`":
            return self.c_backtick(ch)
        self.token.append(ch)

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
        self.stack = Stack()
        self.qstack = Stack()
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

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


def main():
    p = Parser(print)
    p.feed("""
    (add 1 2 "thr\nee")
    (add `,1 ,@'(2 3.14))
    """)
    p.feed(None)


if __name__ == "__main__":
    main()


## EOF
