#!/usr/bin/env python3

"zz.py -- next gen :-)"

## pylint: disable=invalid-name,too-many-lines,unbalanced-tuple-unpacking
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
        frame = stack.pop()
        paramstr = frame.x
        return bounce(frame.c, "(lambda " + paramstr + " " + bodystr + ")")

    def lambda_params_done(self, paramstr):
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
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    def __init__(self, continuation):
        self.continuation = continuation  ## a python func
        self.stack = stack.get()

    def __call__(self, frame):
        (x,) = unpack(frame.x, 1)
        stack.set(self.stack)
        return bounce(self.continuation, x)  ## that's it.


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
## {{{ special forms

def op_define_cont(value):
    frame = stack.pop()
    sym = frame.x
    frame.e.set(sym, value)
    return bounce(frame.c, EL)

@spcl("define")
def op_define(frame):
    sym, defn = unpack(frame.x, 2)

    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")

    stack.push(frame, x=sym)
    return bounce(leval_, Frame(frame, x=defn, c=op_define_cont))


###


def op_if_cont(value):
    frame = stack.pop()
    ca = frame.x
    sexpr = cdr(ca) if value is EL else car(ca)
    return bounce(leval_, Frame(frame, x=sexpr))


@spcl("if")
def op_if(frame):
    p, c, a = unpack(frame.x, 3)
    stack.push(frame, x=cons(c, a))
    return bounce(leval_, Frame(frame, x=p, c=op_if_cont))


###


@spcl("lambda")
def op_lambda(frame):
    params, body = unpack(frame.x, 2)

    if not (isinstance(params, Pair) or params is EL):
        raise TypeError("expected param list, got {params!r}")

    return bounce(frame.c, Lambda(params, body, frame.e))

## this follows https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html
## (special) doesn't quite get the job done due to the way its env works.
## it ain't the same as a recursive scheme macro :-) this quasiquote impl
## is longer than the rest of the special forms combined (!), but it
## helps the bootstrap a lot.

def qq_list_setup(frame, form):
    elt, form = car(form), cdr(form)
    if not (isinstance(form, Pair) or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    stack.push(frame, x=form)
    return bounce(
        qq_list_next, Frame(frame, x=elt, c=qq_list_cont)
    )

def qq_finish(frame, value):
    res = EL if value is SENTINEL else cons(value, EL)
    while True:
        f = stack.pop()
        if f.x is SENTINEL:
            break
        res = cons(f.x, res)
    return bounce(frame.c, res)

def qq_list_cont(value):
    frame = stack.pop()
    form = frame.x

    if form is EL:
        return bounce(qq_finish, frame, value)

    stack.push(frame, x=value)

    return qq_list_setup(frame, form)

def qq_spliced(value):
    frame = stack.pop()
    form = frame.x

    if value is EL:
        if form is EL:
            return bounce(qq_finish, frame, SENTINEL)
        return qq_list_setup(frame, form)

    while value is not EL:
        if not isinstance(value, Pair):
            raise TypeError(f"expected list, got {value!r}")
        elt, value = car(value), cdr(value)
        if value is EL:
            stack.push(frame, x=form)
            return bounce(qq_list_cont, elt)
        stack.push(frame, x=elt)

    raise RuntimeError("logs in the bedpan")

def qq_list_next(frame):
    elt = frame.x

    if isinstance(elt, Pair) and eq(car(elt), symbol("unquote-splicing")):
        _, x = unpack(elt, 2)
        return bounce(leval_, Frame(frame, x=x, c=qq_spliced))
    return bounce(qq, Frame(frame, x=elt, c=qq_list_cont))

def qq_list(self, frame):
    form = frame.x
    app = car(form)

    if eq(app, symbol("quasiquote")):
        _, x = unpack(form, 2)
        return bounce(self.qq, Frame(frame, x=x))

    if eq(app, symbol("unquote")):
        _, x = unpack(form, 2)
        return bounce(leval_, Frame(frame, x=x))

    if eq(app, symbol("unquote-splicing")):
        _, x = unpack(form, 2)
        raise LispError("cannot use unquote-splicing here")

    stack.push(frame, x=SENTINEL)

    return qq_list_setup(frame, form)

def qq(frame):
    form = frame.x
    if isinstance(form, Pair):
        return bounce(qq_list, frame)
    return bounce(frame.c, form)

@spcl("quasiquote")
def op_quasiquote(frame):
    (form,) = unpack(frame.x, 1)
    return bounce(qq, Frame(frame, x=form))

###

@spcl("quote")
def op_quote(frame):
    (x,) = unpack(frame.x, 1)
    return bounce(frame.c, x)

###

def op_setbang_cont(defn):
    frame = stack.pop()
    sym = frame.x
    frame.e.setbang(sym, defn)
    return bounce(frame.c, EL)

@spcl("set!")
def op_setbang(frame):
    sym, defn = unpack(frame.x, 2)
    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")
    stack.push(frame, x=sym)
    return bounce(
        leval_, Frame(frame, x=defn, c=op_setbang_cont)
    )

###

def op_special_cont(value):
    frame = stack.pop()
    sym = frame.x
    if not isinstance(value, Lambda):
        raise TypeError(f"expected lambda, got {value!r}")
    value.special = True
    frame.e.set(sym, value)
    return bounce(frame.c, EL)

@spcl("special")
def op_special(frame):
    sym, defn = unpack(frame.x, 2)

    if not isinstance(sym, Symbol):
        raise TypeError(f"expected symbol, got {sym!r}")

    stack.push(frame, x=sym)
    return bounce(
        leval_, Frame(frame, x=defn, c=op_special_cont)
    )

###

@spcl("trap")
def op_trap(frame):
    (x,) = unpack(frame.x, 1)
    ok = T
    try:
        ## this has to be recursive because you can't pass
        ## exceptions across the trampoline. there is a chance
        ## of blowing the python stack here if you do a deeply
        ## recursive trap.
        res = leval(x, frame.e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return bounce(frame.c, cons(ok, cons(res, EL)))

## }}}


## {{{ XXX
    ## {{{ operators

    def unary(self, g, frame, func):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        return bounce(frame.c, g, func(x))

    def binary(self, g, frame, func):
        ## pylint: disable=no-self-use
        x, y = g.unpack(frame.x, 2)
        return bounce(frame.c, g, func(x, y))

    @glbl(">string")
    def op_to_string(self, g, frame):
        ## pylint: disable=no-self-use
        (x,) = g.unpack(frame.x, 1)
        return bounce(g.stringify_, g, Frame(frame, x=x))

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
            p.feed(rpn.string2str(x))
            p.feed(None)
            x = l[-1] if l else rpn.EL
        e = frame.e
        for _ in range(n_up):
            if rpn.is_empty_list(e):
                raise ValueError(f"cannot go up {n_up} levels")
            e = e.parent()
        return bounce(g.eval_, g, Frame(frame, x=x, e=e))

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

    @glbl("mul")
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

        g.stack.push(frame, x=args)
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

        g.stack.push(frame, x=args)
        return bounce(
            g.stringify_, g, Frame(frame, x=arg, c=self.op_print_cont)
        )

    ###

    @glbl("set-car!")
    def op_setcarbang(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            return rpn.set_car(x, y)

        return self.binary(g, frame, f)

    @glbl("set-cdr!")
    def op_setcdrbang(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            return rpn.set_cdr(x, y)

        return self.binary(g, frame, f)

    @glbl("sub")
    def op_sub(self, g, frame):
        rpn = g.rpn

        def f(x, y):
            if not (rpn.is_number(x) and rpn.is_number(y)):
                raise TypeError(f"expected numbers, got {x!r} and {y!r}")
            return x - y  ## XXX implicit conversion from python to lisp

        return self.binary(g, frame, f)

    @glbl("type")
    def op_type(self, g, frame):
        def f(x):
            t = g.type(x)
            if t is not SENTINEL:
                return t
            if is_lambda(x):
                return g.symbol("lambda")
            if is_continuation(x):
                return g.symbol("continuation")
            if callable(x):
                return g.symbol("primitive")
            return g.symbol("opaque")

        return self.unary(g, frame, f)

    ## }}}
    ## {{{ ffi

    def module_ffi(self, g, args, module):
        ## pylint: disable=no-self-use
        if not args:
            raise TypeError("at least one arg required")
        sym = args.pop(0)
        if not g.rpn.is_symbol(sym):
            raise TypeError(f"expected symbol, got {sym!r}")
        func = getattr(module, g.rpn.sym2str(sym), None)
        if func is None:
            raise ValueError(f"function {sym!r} does not exist")
        return func(*args)

    @ffi("math")
    def op_ffi_math(self, g, args):
        import math  ## pylint: disable=import-outside-toplevel

        return self.module_ffi(g, args, math)

    @ffi("random")
    def op_ffi_random(self, g, args):
        import random  ## pylint: disable=import-outside-toplevel

        return self.module_ffi(g, args, random)

    @ffi("range")
    def op_ffi_range(self, _, args):
        ## pylint: disable=no-self-use
        return list(range(*args))

    @ffi("shuffle")
    def op_ffi_shuffle(self, _, args):
        ## pylint: disable=no-self-use
        import random  ## pylint: disable=import-outside-toplevel

        (l,) = args
        random.shuffle(l)
        return l

    @ffi("time")
    def op_ffi_time(self, g, args):
        import time  ## pylint: disable=import-outside-toplevel

        def f(args):
            ret = []
            for arg in args:
                if isinstance(arg, list):
                    arg = tuple(arg)
                ret.append(arg)
            return ret

        return self.module_ffi(g, f(args), time)

    ## }}}


## }}}


## EOF
