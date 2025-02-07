#!/usr/bin/env python3

"new scanner test"

## pylint: disable=invalid-name,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import locale
import os
import re
import sys
import traceback


class error(Exception):
    pass


## {{{ scanner 2


class Scanner:
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

    def __init__(self, callback):
        self.callback = callback
        self.interesting = re.compile(r"(,@|[][()'`,])\s*").match
        self.sym = re.compile(r"([^][\s()'`,;\"]+)\s*").match
        self.lut = {
            "(": self.T_LPAR,
            ")": self.T_RPAR,
            "[": self.T_LPAR,
            "]": self.T_RPAR,
            "'": self.T_TICK,
            "`": self.T_BACKTICK,
            ",": self.T_COMMA,
            ",@": self.T_COMMA_AT,
        }

    def feed(self, text):
        if text is None:
            self.push(self.T_EOF, None)
            return
        text = text.strip().replace("\r\n", "\n").replace("\r", "\n")
        while text:
            m = self.interesting(text)
            if m:
                g = m.group(1)
                text = text[m.end(0) :]
                self.push(self.lut[g], None)
            elif text.startswith(";"):
                p = text.find("\n")
                if p < 0:
                    return
                text = text[p + 1 :].lstrip()
            elif text.startswith('"'):
                p = text.find('"', 1)
                if p < 0:
                    raise SyntaxError()
                ## XXX handle escapes
                s = text[1:p]
                self.push(self.T_STR, s)
                text = text[p + 1 :].lstrip()
            else:
                m = self.sym(text)
                assert m
                token = m.group(1)
                text = text[m.end(0) :]
                self.push(self.T_SYM, token)

    def push(self, ttype, token):
        if ttype == self.T_SYM:
            if not token:
                return
            try:
                token = int(token, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    token = float(token)
                    ttype = self.T_REAL
                except:  ## pylint: disable=bare-except
                    pass  ## value error, range error
        self.callback(ttype, token)


## }}}
## {{{ parser


class Parser:
    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def process_token(self, ttype, token):
        ## pylint: disable=too-many-branches
        if ttype == self.scanner.T_SYM:
            self.add(symbol(token))
        elif ttype == self.scanner.T_LPAR:
            self.stack.append(Queue())
        elif ttype == self.scanner.T_RPAR:
            if not self.stack:
                raise SyntaxError("too many ')'s")
            l = self.stack.pop().head()
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
        elif ttype == self.scanner.T_EOF:
            if self.stack:
                raise SyntaxError("premature eof in '('")
        else:
            raise RuntimeError((ttype, token))

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(x)


## }}}
## {{{ high level parsing routines


def parse(text, callback):
    p = Parser(callback)
    p.feed(text)
    p.feed(None)


def load(filename, callback):
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
        parse(fp.read(), callback)


## }}}
## {{{ basics

EL = object()
T = True
SENTINEL = object()


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["s"]

    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s


ST = {}


def symbol(s):
    return ST.setdefault(s, Symbol(s))


def atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return atom(x) and x is y


## }}}
## {{{ queue


class Queue:
    def __init__(self):
        self.h = self.t = EL

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
## {{{ env and globals


class Env(dict):
    __slots__ = ["p"]

    def __init__(self, params, args, parent):
        super().__init__()
        self.p = parent
        variadic = False
        while params is not EL:
            param, params = params
            if eq(param, symbol("&")):
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("junk after &")
                self[param] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                arg, args = args
                self[param] = arg
        if variadic:
            raise SyntaxError("args end with &")
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, key, default):
        if not isinstance(key, Symbol):
            raise TypeError(key)
        e = self
        while e is not SENTINEL:
            x = dict.get(e, key, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        return default

    def set(self, key, value):
        if not isinstance(key, Symbol):
            raise TypeError(key)
        self[key] = value
        return EL

    def setbang(self, key, value):
        if not isinstance(key, Symbol):
            raise TypeError(key)
        e = self
        while e is not SENTINEL:
            x = dict.get(e, key, SENTINEL)
            if x is not SENTINEL:
                e[key] = value
                return EL
            e = e.p
        raise NameError(key)


GLB = Env(EL, EL, SENTINEL)
GLB.set(symbol("#t"), T)


def glbl(name):
    def wrap(func):
        GLB.set(symbol(name), func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        GLB.set(symbol(name), func)
        func.special = True
        return func

    return wrap


## }}}
## {{{ lambda


class Lambda:
    special = False

    def __init__(self, params, body, env):
        self.p, self.b, self.e = params, body, env

    def __call__(self, args, env):
        p = env if self.special else self.e
        e = Env(self.p, args, p)
        return leval(self.b, e)

    def __repr__(self):
        return "(lambda " + stringify(self.p) + " " + stringify(self.b) + ")"


## }}}
## {{{ stringify


def stringify(x):
    ## pylint: disable=too-many-return-statements
    if x is EL:
        return "()"
    if x is T:
        return "#t"
    if isinstance(x, (Symbol, Lambda, int, float, str)):
        return repr(x)
    if not isinstance(x, list):
        if callable(x):
            return "[primitive]"
        return "[opaque]"
    parts = []
    while x is not EL:
        y, x = x
        parts.append(stringify(y))
    return "(" + " ".join(parts) + ")"


## }}}
## {{{ eval


def leval(x, e=SENTINEL):
    ## pylint: disable=too-many-branches
    e = GLB if e is SENTINEL else e
    if isinstance(x, Symbol):
        obj = e.get(x, SENTINEL)
        if obj is SENTINEL:
            raise NameError(x)
        return obj
    if isinstance(x, list):
        sym, args = x
    elif isinstance(x, Lambda):
        sym = x
        args = EL
    else:
        return x
    if isinstance(sym, Symbol):
        op = e.get(sym, SENTINEL)
        if op is not SENTINEL and getattr(op, "special", False):
            return op(args, e)
        proc = leval(sym, e)
    elif callable(sym):
        proc = sym
    elif not isinstance(sym, list):
        raise TypeError(f"expected proc/list, got {sym!r}")
    else:
        proc = leval(sym, e)
    if not callable(proc):
        raise TypeError(f"expected proc, got {proc!r}")

    q = Queue()
    while args is not EL:
        arg, args = args
        q.enqueue(leval(arg, e))
    return proc(q.head(), e)


## }}}
## {{{ unpack


def unpack(args, n):
    ret = []
    for _ in range(n):
        if args is EL:
            raise TypeError(f"not enough args, need {n}")
        arg, args = args
        ret.append(arg)
    if args is not EL:
        raise TypeError(f"too many args, need {n}")
    return ret


## }}}
## {{{ special forms


@spcl("cond")
def op_cond(args, e):
    while args is not EL:
        arg, args = args
        predicate, consequent = unpack(arg, 2)
        if leval(predicate, e) is not EL:
            return leval(consequent, e)
    return EL


@spcl("define")
def op_define(args, e):
    name, value = unpack(args, 2)
    return e.set(name, leval(value, e))


@spcl("if")
def op_if(args, e):
    p, c, a = unpack(args, 3)
    return leval(a, e) if leval(p, e) is EL else leval(c, e)


@spcl("lambda")
def op_lambda(args, e):
    params, body = unpack(args, 2)
    return Lambda(params, body, e)


@spcl("quote")
def op_quote(args, _):
    (x,) = unpack(args, 1)
    return x


@spcl("set!")
def op_setbang(args, e):
    name, value = unpack(args, 2)
    e.setbang(name, leval(value, e))
    return EL


@spcl("special")
def op_special(args, e):
    name, value = unpack(args, 2)
    value = leval(value, e)
    value.special = True
    return e.set(name, value)


## }}}
## {{{ procedures


def unary(args, f):
    (x,) = unpack(args, 1)
    return f(x)


def binary(args, f):
    x, y = unpack(args, 2)
    return f(x, y)


@glbl("add")
def op_add(args, _):
    return binary(args, lambda x, y: x + y)


@glbl("atom?")
def op_atom(args, _):
    (x,) = unpack(args, 1)
    return T if atom(x) else EL


@glbl("car")
def op_car(args, _):
    return unary(args, lambda x: x[0])


@glbl("cdr")
def op_cdr(args, _):
    return unary(args, lambda x: x[1])


@glbl("cons")
def op_cons(args, _):
    return binary(args, lambda x, y: [x, y])


@glbl("div")
def op_div(args, _):
    def f(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(args, f)


@glbl("eq?")
def op_eq(args, _):
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(args, f)


@glbl("equal?")
def op_equal(args, _):
    def f(x, y):
        return T if x == y else EL

    return binary(args, f)


@glbl("error")
def op_error(args, e):
    (x,) = unpack(args, 1)
    raise error(leval(x, e))


@glbl("eval")
def op_eval(args, e):
    try:
        x, n_up = unpack(args, 2)
    except TypeError:
        (x,) = unpack(args, 1)
        n_up = 0
    if isinstance(x, str):
        l = []
        parse(x, lambda expr: l.append(leval(expr, e)))
        x = l[-1] if l else EL
    for _ in range(n_up):
        e = e.p
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
    return leval(x, e)


@glbl("exit")
def op_exit(args, _):
    (x,) = unpack(args, 1)
    if isinstance(x, int):
        raise SystemExit(x)
    raise SystemExit(stringify(x))


@glbl("last")
def op_last(args, _):
    (x,) = unpack(args, 1)
    ret = EL
    while x is not EL:
        ret, x = x
    return ret


@glbl("lt?")
def op_lt(args, _):
    def f(x, y):
        return T if x < y else EL

    return binary(args, f)


@glbl("mul")
def op_mul(args, _):
    return binary(args, lambda x, y: x * y)


@glbl("nand")
def op_nand(args, _):
    def f(x, y):
        return ~(x & y)

    return binary(args, f)


@glbl("null?")
def op_null(args, _):
    (x,) = unpack(args, 1)
    return T if x is EL else EL


@glbl("print")
def op_print(args, _):
    if args is EL:
        print()
        return EL
    end = " "
    while args is not EL:
        x, args = args
        if args is EL:
            end = "\n"
        print(stringify(x), end=end)
    return EL


@glbl("set-car!")
def op_setcar(args, _):
    x, y = unpack(args, 2)
    x[0] = y
    return EL


@glbl("set-cdr!")
def op_setcdr(args, _):
    x, y = unpack(args, 2)
    x[1] = y
    return EL


@glbl("sub")
def op_sub(args, _):
    return binary(args, lambda x, y: x - y)


@glbl("trap")
def op_trap(args, e):
    (x,) = unpack(args, 1)
    ok = T
    try:
        res = leval(x, e)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    return [ok, [res, EL]]


@glbl("type")
def op_type(args, _):
    ## pylint: disable=too-many-return-statements
    (x,) = unpack(args, 1)
    if x is EL:
        return symbol("()")
    if x is T:
        return symbol("#t")
    if isinstance(x, Symbol):
        return symbol("symbol")
    if isinstance(x, int):
        return symbol("int")
    if isinstance(x, float):
        return symbol("float")
    if isinstance(x, str):
        return symbol("str")
    if isinstance(x, Lambda):
        return symbol("lambda")
    if not isinstance(x, list):
        if callable(x):
            return symbol("primitive")
        return symbol("opaque")
    return symbol("list")


@glbl("while")
def op_while(args, e):
    (x,) = unpack(args, 1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    while leval(x, e) is not EL:
        pass
    return EL


## }}}
## {{{ main repl


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


def main():
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
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if not stop:
        raise SystemExit(repl(callback))


## }}}
## {{{ stdlib


def boot():
    ## pylint: disable=line-too-long
    parse(
        """
(define list? (lambda (x) (if (eq? (type x) (quote list)) #t ())))
(define list (lambda (& args) args))
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define foreach (lambda (f l)
    (while (lambda ()
        (if
            (null? l)
            ()
            ((lambda (x y z) z)  ;; (do) without (if) since args eval l->r
                (f (car l))
                (set! l (cdr l))
                #t
            )
        )
    ))
))
(define do (lambda (& args) (last args)))
(define reverse (lambda (l) (do
    (define r ())
    (while (lambda ()
        (if
            (null? l)
            ()
            (do
                (set! r (cons (car l) r))
                (set! l (cdr l))
                #t
            )
        )
    ))
    r
)))
(special and (lambda (__special_and2_x__ __special_and2_y__)
    (if (eval __special_and2_x__ 1) (eval __special_and2_y__ 1) ())
))
(special or (lambda (__special_or2_x__ __special_or2_y__)
    (if (eval __special_or2_x__ 1) #t (eval __special_or2_y__ 1))
))
(define join (lambda (x y)
    (if (null? x) y (cons (car x) (join (cdr x) y)))
))
;;; definition of let sicp p.87
(special let (lambda (__special_let_vars__ __special_let_body__) (do
    ;; (let ((x a) (y b)) body)

    (define __special_let_vdecls__ ())  ;; (x y)
    (define __special_let_vvals__ ())   ;; ((eval a) (eval b))
    ;; declare one var
    (define __special_let_decl1__ (lambda (__special_let_decl1_var__ __special_let_decl1_value__) (do
        (set! __special_let_vdecls__ (join __special_let_vdecls__ (list __special_let_decl1_var__)))
        (set! __special_let_vvals__  (join __special_let_vvals__  (list (eval __special_let_decl1_value__))))
    )))
    ;; declare the next item from vars
    (define __special_let_next__ (lambda () (do
        (__special_let_decl1__ (car (car __special_let_vars__)) (car (cdr (car __special_let_vars__))))
        (set! __special_let_vars__ (cdr __special_let_vars__))
    )))
    ;; declare everthing, then return (doit)
    (define __special_let_decls__ (lambda () (
        cond
            ((null? __special_let_vars__)   (__special_let_doit__))
            (#t (do
                    (__special_let_next__) (__special_let_decls__)
            ))
    )))
    (define __special_let_doit__ (lambda () (do
        (define __special_let_doit_head__ (join (list (quote lambda)) (list __special_let_vdecls__)))
        (define __special_let_doit_mid__  (join __special_let_doit_head__ (list __special_let_body__)))
        (define __special_let_doit_lam__  (join (list __special_let_doit_mid__) __special_let_vvals__))
        (eval __special_let_doit_lam__ 1)
    )))
    (__special_let_decls__)
)))
(define length (lambda (l) (do
    (define n 0)
    (while (lambda ()
        (if
            (null? l)
            ()
            (do
                (set! n (add n 1))
                (set! l (cdr l))
                #t
            )
        )
    ))
    n
)))
(define neg (lambda (x) (sub 0 x)))
(define add (lambda (x y) (sub x (neg y))))
(define mod (lambda (n d) (sub n (mul d (div n d)))))
(define abs (lambda (x) (if (lt? x 0) (neg x) x)))
(define copysign (lambda (x y) (if (lt? y 0) (neg (abs x)) (abs x))))
(define le? (lambda (x y) (or (lt? x y) (equal? x y))))
(define ge? (lambda (x y) (not (lt? x y))))
(define gt? (lambda (x y) (not (le? x y))))
(define bnot (lambda (x) (nand x x)))
(define band (lambda (x y) (bnot (nand x y))))
(define bor  (lambda (x y) (nand (bnot x) (bnot y))))
(define bxor (lambda (x y) (band (nand x y) (bor x y))))

;; signed integer multiplication from subtraction and right shift (division)
(define umul (lambda (x y accum)
    (if
        (while (lambda ()  ;; <= this is where it's not portable
            (if
                (equal? 0 x)
                ()
                ((lambda (& _) #t)
                    (if
                        (equal? (band x 1) 1)
                        (set! accum (add accum y))
                        ()
                    )
                    (set! x (div x 2))
                    (set! y (mul y 2))
                )
            )
        ))
        ()
        accum
    )
))
(define smul (lambda (x y) (do
    (define sign 1)
    (if (lt? x 0) (set! sign (neg sign)) ())
    (if (lt? y 0) (set! sign (neg sign)) ())
    (cond
        ((equal? x 0)       0)
        ((equal? y 0)       0)
        ((equal? (abs y) 1) (copysign x sign))
        ((lt? y x)          (copysign (umul (abs y) (abs x) 0) sign))
        (#t                 (copysign (umul (abs x) (abs y) 0) sign))
    )
)))
    """,
        leval,
    )


boot()


## }}}


if __name__ == "__main__":
    main()
