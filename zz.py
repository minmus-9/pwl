#!/usr/bin/env python3

"zz.py -- next gen :-)"

## pylint: disable=invalid-name,too-many-lines
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


## {{{ trampoline


def trampoline(func, *args):
    while True:
        ret = func(*args)
        if len(ret) == 1:
            return ret[0]
        func, args = ret


def bounce(func, *args):
    return func, args


def land(value):
    return (value,)


## }}}
## {{{ basics


class LispError(Exception):
    ...


SENTINEL = object()


## }}}
## {{{ atoms


class EL_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "()"


EL = EL_()
del EL_


class T_:
    ## pylint: disable=too-few-public-methods

    def __repr__(self):
        return "#t"


T = T_()
del T_


class Symbol:
    ## pylint: disable=too-few-public-methods

    def __init__(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        self.s = s

    def __str__(self):
        return self.s


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        return self.t.setdefault(s, Symbol(s))


symbol = SymbolTable().symbol


def is_atom(x):
    return x is EL or isinstance(x, Symbol) or x is T


def eq(x, y):
    return is_atom(x) and x is y


## }}}
## {{{ pair


class Pair(list):
    ## pylint: disable=too-few-public-methods

    def __init__(self, x, y):
        super().__init__()
        self[:] = (x, y)


def cons(x, y):
    return Pair(x, y)


def car(x):
    if not isinstance(x, Pair):
        raise TypeError(f"expected pair, got {x!r}")
    return x[0]


def cdr(x):
    if x is EL:
        return EL
    if not isinstance(x, Pair):
        raise TypeError(f"expected () or pair, got {x!r}")
    return x[1]


def set_car(x, y):
    if not isinstance(x, Pair):
        raise TypeError(f"expected () or pair, got {x!r}")
    x[0] = y
    return EL


def set_cdr(x, y):
    if not isinstance(x, Pair):
        raise TypeError(f"expected () or pair, got {x!r}")
    x[1] = y
    return EL


## }}}
## {{{ stack


class Stack:
    def __init__(self):
        self.s = []

    def __bool__(self):
        return bool(self.s)

    def clear(self):
        self.s.clear()

    def push(self, thing):
        self.s.append(thing)

    def pop(self):
        if not self.s:
            raise ValueError("stack is empty")
        return self.s.pop()

    def top(self):
        if not self.s:
            raise ValueError("stack is empty")
        return self.s[-1]

    ## for continuations

    def get(self):
        return self.s

    def set(self, value):
        self.s = value


## }}}
## {{{ stack frame


class Frame:
    ## pylint: disable=too-few-public-methods

    def __init__(self, f, **kw):
        assert isinstance(f, Frame) or f is SENTINEL
        if f is not SENTINEL:
            self.__dict__.update(f.__dict__)
        self.__dict__.update(kw)


## }}}
## {{{ frame stack and global stack


class FrameStack(Stack):
    def push(self, thing, **kw):
        super().push(Frame(thing, **kw))


stack = FrameStack()


## }}}
## {{{ queue


class Queue:
    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

    def head(self):
        return self.h

    def enqueue(self, x):
        node = cons(x, EL)
        if self.h is EL:
            self.h = node
        else:
            set_cdr(self.t, node)
        self.t = node

    def dequeue(self):
        node = self.h
        if node is EL:
            raise ValueError("queue is empty")
        h = self.h = cdr(node)
        if h is EL:
            self.t = EL
        return car(node)


## }}}
## {{{ environment and global genv


class Environment:
    def __init__(self, params, args, parent):
        assert isinstance(parent, Environment) or parent is SENTINEL
        self.p = parent
        self.d = {}
        self.bind(params, args)

    def bind(self, params, args):
        pl, al = params, args
        variadic = False
        while params is not EL:
            p, params = car(params), cdr(params)
            if not isinstance(p, Symbol):
                raise TypeError(
                    f"expected symbol, got {p!r} at {pl!r} <= {al!r}"
                )
            if eq(p, symbol("&")):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError(f"extra junk {params!r} after '&'")
                self.d[p] = args
                return
            elif args is EL:
                raise TypeError(f"not enough args at {pl!r} <= {al!r}")
            else:
                a, args = car(args), cdr(args)
                self.d[p] = a
        if variadic:
            raise SyntaxError(f"'&' ends param list {pl!r} <= {al!r}")
        if args is not EL:
            raise TypeError(f"too many args at {pl!r} <= {al!r}")

    def get(self, sym):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            x = e.d.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        return SENTINEL

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        self.d[sym] = value
        return EL

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            if sym in e.d:
                e.d[sym] = value
                return EL
            e = e.p
        raise NameError(str(sym))


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


## }}}
## {{{ primitive definition decorators


def glbl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.special = True
        return func

    return wrap


def ffi(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.ffi = True
        return func

    return wrap


## }}}
## {{{ scanner


class Scanner:
    ##  pylint: disable=too-many-instance-attributes

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
        self.token = []
        self.add = self.token.append
        self.pos = 0
        self.state = self.S_SYM
        self.stack = ""
        self.c_map = {
            "(": self.c_lpar,
            ")": self.c_rpar,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_cmnt,
            "'": self.c_tick,
            "`": self.c_backtick,
            ",": self.c_comma,
            '"': self.c_quote,
        }
        self.lookup = self.c_map.get
        self.s_map = {
            self.S_BS: self.s_bs,
            self.S_COMMA: self.s_comma,
            self.S_CMNT: self.s_comment,
            self.S_STR: self.s_str,
            self.S_SYM: self.s_sym,
        }

    def c_backtick(self):
        if self.token:
            raise SyntaxError("backtick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_BACKTICK)

    def c_comma(self):
        if self.token:
            raise SyntaxError("comma is not a delimiter")
        self.state = self.S_COMMA

    def c_cmnt(self):
        self.state = self.S_CMNT

    def c_lbrack(self):
        self.stack += "]"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_lpar(self):
        self.stack += ")"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_quote(self):
        if self.token:
            raise SyntaxError("quote is not a delimiter")
        self.state = self.S_STR

    def c_rbrack(self):
        if not self.stack:
            raise SyntaxError("too many ']'")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != "]":
            raise SyntaxError(f"expected {c!r}, got ']'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_rpar(self):
        if not self.stack:
            raise SyntaxError("too many ')'")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != ")":
            raise SyntaxError(f"expected {c!r}, got ')'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_tick(self):
        if self.token:
            raise SyntaxError("tick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_TICK)

    def c_ws(self):
        self.push(self.T_SYM)

    def s_bs(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError("bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STR

    def s_comma(self, ch):
        if ch == "@":
            self.push(self.T_COMMA_AT)
        else:
            self.push(self.T_COMMA)
            self.pos -= 1
        self.state = self.S_SYM

    def s_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def s_str(self, ch):
        if ch == '"':
            self.state = self.S_SYM
            self.push(self.T_STR)
        elif ch == "\\":
            self.state = self.S_BS
        else:
            self.add(ch)

    def s_sym(self, ch):
        if ch in " \n\r\t":
            self.push(self.T_SYM)
        else:
            f = self.lookup(ch)
            if f:
                f()
            else:
                self.add(ch)

    def eof(self):
        if self.stack:
            raise SyntaxError(f"eof in {self.stack[-1]!r}")
        self.push(self.T_SYM)
        self.push(self.T_EOF)

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            return self.eof()
        self.pos, n = 0, len(text)
        while self.pos < n:
            ch = text[self.pos]
            self.pos += 1
            self.s_map[self.state](ch)

    def push(self, ttype):
        t = "".join(self.token)
        self.token.clear()
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


## }}}
## {{{ parser


class Parser:
    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.q_map = {  ## could if-away these special cases but just use dict
            symbol("'"): symbol("quote"),
            symbol("`"): symbol("quasiquote"),
            symbol(","): symbol("unquote"),
            symbol(",@"): symbol("unquote-splicing"),
        }

    def process_token(self, ttype, token):
        ## pylint: disable=too-many-branches
        ## ugly, but the quickest to write
        if ttype == self.scanner.T_SYM:
            self.add(symbol(token))
        elif ttype == self.scanner.T_LPAR:
            self.stack.append(Queue())
        elif ttype == self.scanner.T_RPAR:
            if not self.stack:
                raise SyntaxError("too many ')'s")
            q = self.stack.pop()
            l = self.filter(q.head())
            if not self.stack:
                self.callback(l)
            else:
                self.add(l)
        elif ttype in (
            self.scanner.T_INT,
            self.scanner.T_REAL,
            self.scanner.T_STR,
        ):
            self.add(token)
        elif ttype == self.scanner.T_TICK:
            self.add(symbol("'"))
        elif ttype == self.scanner.T_BACKTICK:
            self.add(symbol("`"))
        elif ttype == self.scanner.T_COMMA:
            self.add(symbol(","))
        elif ttype == self.scanner.T_COMMA_AT:
            self.add(symbol(",@"))
        elif ttype == self.scanner.T_EOF:
            if self.stack:
                raise SyntaxError("premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(x)

    def filter(self, sexpr):
        ## pylint: disable=no-self-use
        "process ' ` , ,@"
        q = Queue()

        ## NB we know this is a well-formed list
        while sexpr is not EL:
            elt, sexpr = car(sexpr), cdr(sexpr)
            if isinstance(elt, Symbol) and elt in self.q_map:
                elt, sexpr = self.process_syms(elt, sexpr)
            q.enqueue(elt)
        return q.head()

    def process_syms(self, elt, sexpr):
        replacement = self.q_map[elt]
        if sexpr is EL:
            raise SyntaxError(f"got {elt!r} at end of list")
        quoted, sexpr = car(sexpr), cdr(sexpr)
        if isinstance(quoted, Symbol) and quoted in self.q_map:
            quoted, sexpr = self.process_syms(quoted, sexpr)
        elt = cons(replacement, cons(quoted, EL))
        return elt, sexpr


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
        for d in [os.path.dirname(__file__)] + sys.path:
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
## {{{ unpack


def unpack(args, n):
    al = args
    ret = []
    for _ in range(n):
        if args is EL:
            raise TypeError(f"not enough args, need {n} from {al!r}")
        if not isinstance(args, Pair):
            raise TypeError(f"malformed args, need {n} from {al!r}")
        ret.append(car(args))
        args = cdr(args)
    if args is not EL:
        raise TypeError(f"too many args, need {n} from {al!r}")
    return ret


## }}}
## {{{ repl and main


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
        except:  ## pylint: disable=bare-except
            p = Parser(callback)
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
    try:
        sys.set_int_max_str_digits(0)
    except AttributeError:
        pass

    def callback(sexpr):
        try:
            value = leval(sexpr)
        except SystemExit:
            raise
        except:
            print("Offender (pyth):", sexpr)
            print("Offender (lisp):", stringify(sexpr), "\n")
            raise
        if value is not EL:
            print(stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "+":
            continue  ## ignore for compatibility
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if force_repl or not stop:
        raise SystemExit(repl(callback))


## }}}
## {{{ lambda


class Lambda:
    special = False

    def __init__(self, params, body, env):
        self.p, self.b, self.e = params, body, env

    def __call__(self, frame):
        args = frame.x
        p = frame.e if self.special else self.e
        e = Environment(self.p, args, p)
        return bounce(leval_, Frame(frame, x=self.b, e=e))

    ###

    def lambda_body_done(self, bodystr):
        ## pylint: disable=no-self-use
        frame = stack.pop()
        paramstr = frame.x
        return bounce(frame.c, "(lambda " + paramstr + " " + bodystr + ")")

    def lambda_params_done(self, paramstr):
        ## pylint: disable=no-self-use
        frame = stack.pop()
        body = frame.x
        stack.push(frame, x=paramstr)
        return bounce(
            stringify_, Frame(frame, x=body, c=self.lambda_body_done)
        )

    def stringify_(self, frame):
        stack.push(frame, x=self.b)
        return bounce(
            stringify_,
            Frame(frame, x=self.p, c=self.lambda_params_done),
        )


## }}}


## EOF
