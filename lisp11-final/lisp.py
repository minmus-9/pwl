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

"lisp.py -- lcore demo"

## {{{ prologue
## pylint: disable=invalid-name, too-many-lines
## XXX pylint: disable=missing-docstring

import sys

from lcore import (
    main as lmain,
    Context,
    EL,
    SENTINEL,
    Symbol,
    T,
    car,
    cdr,
    cons,
    create_continuation,
    create_lambda,
    eq,
    error,
    ffi,
    glbl,
    is_atom,
    k_leval,
    k_stringify,
    parse,
    set_car,
    set_cdr,
    spcl,
    symcheck,
)


## }}}
## {{{ helpers


def unary(ctx, f):
    ctx.val = f(ctx.unpack1())
    return ctx.cont


def binary(ctx, f):
    ctx.val = f(*ctx.unpack2())
    return ctx.cont


## }}}
## {{{ special forms


@spcl("cond")
def op_cond(ctx):
    ctx.push_ce()
    return op_cond_setup(ctx, ctx.argl)


def op_cond_setup(ctx, args):
    if args is EL:
        ctx.pop_ce()
        ctx.val = EL
        return ctx.cont

    ctx.env = ctx.top()

    pc, args = args
    try:
        ctx.exp, c = pc
        if c.__class__ is not list:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected list, got {pc!r}") from None
    if cdr(c) is EL:
        c = car(c)
    else:
        c = cons(ctx.symbol("do"), c)
    ctx.push(c)
    ctx.push(args)
    ctx.cont = op_cond_next
    return k_leval


def op_cond_next(ctx):
    args = ctx.pop()
    ctx.exp = ctx.pop()
    if ctx.val is EL:
        return op_cond_setup(ctx, args)
    ctx.pop_ce()
    return k_leval


@spcl("define")
def op_define(ctx):
    try:
        sym, body = ctx.argl
        if body is EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("define takes at leats 2 args") from None

    if sym.__class__ is list:
        sym, params = sym
        if cdr(body) is EL:
            body = car(body)
        else:
            body = cons(ctx.symbol("do"), body)
        ctx.env[symcheck(sym)] = create_lambda(params, body, ctx.env)
        ctx.val = EL
        return ctx.cont

    if cdr(body) is not EL:
        raise SyntaxError("body must be a single value")
    ctx.push_ce()
    ctx.push(symcheck(sym))
    ctx.exp = car(body)
    ctx.cont = k_op_define
    return k_leval


def k_op_define(ctx):
    sym = ctx.pop()
    ctx.pop_ce()
    ctx.env[sym] = ctx.val
    ctx.val = EL
    return ctx.cont


@spcl("if")
def op_if(ctx):
    p, c, a = ctx.unpack3()
    ctx.push_ce()
    ctx.push((c, a))
    ctx.exp = p
    ctx.cont = k_op_if
    return k_leval


def k_op_if(ctx):
    c, a = ctx.pop()
    ctx.pop_ce()
    ctx.exp = a if ctx.val is EL else c
    return k_leval


@spcl("lambda")
def op_lambda(ctx):
    try:
        params, body = ctx.argl
        if body.__class__ is not list:
            raise TypeError()
    except TypeError:
        raise SyntaxError("expected at least 2 args") from None
    if cdr(body) is EL:
        body = car(body)
    else:
        body = cons(ctx.symbol("do"), body)
    ctx.val = create_lambda(params, body, ctx.env)
    return ctx.cont


@spcl("quote")
def op_quote(ctx):
    ctx.val = ctx.unpack1()
    return ctx.cont


@spcl("set!")
def op_setbang(ctx):
    sym, value = ctx.unpack2()
    ctx.push(symcheck(sym))
    ctx.push_ce()
    ctx.cont = k_op_setbang
    ctx.exp = value
    return k_leval


def k_op_setbang(ctx):
    ctx.pop_ce()
    sym = ctx.pop()
    e = ctx.env
    while e is not SENTINEL:
        if sym in e:
            e[sym] = ctx.val
            ctx.val = EL
            return ctx.cont
        e = e[SENTINEL]
    raise NameError(str(sym))


@spcl("special")
def op_special(ctx):
    try:
        sym, body = ctx.argl
        if body is EL:
            raise TypeError()
    except TypeError:
        raise SyntaxError("define takes at leats 2 args") from None

    if sym.__class__ is list:
        sym, params = sym
        if cdr(body) is EL:
            body = car(body)
        else:
            body = cons(ctx.symbol("do"), body)
        lam = create_lambda(params, body, ctx.env)
        lam.special = True
        ctx.env[symcheck(sym)] = lam
        ctx.val = EL
        return ctx.cont

    if cdr(body) is not EL:
        raise SyntaxError("body must be a single value")
    ctx.push_ce()
    ctx.push(symcheck(sym))
    ctx.exp = car(body)
    ctx.cont = k_op_special
    return k_leval


def k_op_special(ctx):
    sym = ctx.pop()
    ctx.pop_ce()
    proc = ctx.val
    if not callable(proc):
        raise SyntaxError("expected proc")
    proc.special = True
    ctx.env[sym] = proc
    ctx.val = EL
    return ctx.cont


@spcl("trap")
def op_trap(ctx):
    x = ctx.unpack1()
    ok = T
    ctx.push_ce()
    try:
        res = ctx.leval(x, ctx.env)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    ctx.pop_ce()
    ctx.val = cons(ok, cons(res, EL))
    return ctx.cont


## }}}
## {{{ quasiquote


@spcl("quasiquote")
def op_quasiquote(ctx):
    ctx.exp = ctx.unpack1()
    return qq_


def qq_(ctx):
    form = ctx.exp
    if form.__class__ is not list:
        ctx.val = form
        return ctx.cont
    app = form[0]
    if eq(app, ctx.symbol("quasiquote")):
        ## XXX proper nesting?
        ctx.argl = form[1]
        return op_quasiquote
    if eq(app, ctx.symbol("unquote")):
        ctx.argl = form
        _, ctx.exp = ctx.unpack2()
        return k_leval
    if eq(app, ctx.symbol("unquote-splicing")):
        _, __ = ctx.unpack2()
        raise SyntaxError("cannot use unquote-splicing here")
    ctx.push_ce()
    ctx.push(SENTINEL)
    return k_qq_setup(ctx, form)


def k_qq_setup(ctx, form):
    elt, form = form
    if not (form.__class__ is list or form is EL):
        raise TypeError(f"expected list, got {form!r}")
    ctx.push(form)
    ctx.push_ce()
    if elt.__class__ is list and elt[0] is ctx.symbol("unquote-splicing"):
        ctx.argl = elt
        _, ctx.exp = ctx.unpack2()
        ctx.cont = k_qq_spliced
        return k_leval
    ctx.cont = k_qq_next
    ctx.exp = elt
    return qq_


def k_qq_spliced(ctx):
    ctx.pop_ce()
    form = ctx.pop()
    value = ctx.val
    if value is EL:
        if form is EL:
            return k_qq_finish
        return k_qq_setup(ctx, form)
    while value is not EL:
        if value.__class__ is not list:
            raise TypeError(f"expected list, got {value!r}")
        elt, value = value
        if value is EL:
            ctx.val = elt
            ctx.push(form)
            ctx.push_ce()
            return k_qq_next
        ctx.push(elt)
    raise RuntimeError("bugs in the taters")


def k_qq_next(ctx):
    ctx.pop_ce()
    form = ctx.pop()
    ctx.push(ctx.val)
    if form is EL:
        return k_qq_finish
    return k_qq_setup(ctx, form)


def k_qq_finish(ctx):
    ret = EL
    while True:
        x = ctx.pop()
        if x is SENTINEL:
            break
        ret = [x, ret]
    ctx.pop_ce()
    ctx.val = ret
    return ctx.cont


## }}}
## {{{ primitives


@glbl("apply")
def op_apply(ctx):
    proc, ctx.argl = ctx.unpack2()
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError(f"expected proc, got {proc!r}") from None
    return proc


@glbl("atom?")
def op_atom(ctx):
    return unary(ctx, op_atom_f)


def op_atom_f(x):
    return T if is_atom(x) else EL


@glbl("begin")
@glbl("do")
def op_do(ctx):
    ctx.val = EL
    args = ctx.argl
    try:
        while args is not EL:
            ctx.val, args = args
    except TypeError:
        raise SyntaxError("expected list") from None
    return ctx.cont


@glbl("call/cc")
@glbl("call-with-current-continuation")
def op_callcc(ctx):
    proc = ctx.unpack1()
    try:
        _ = proc.__call__
    except AttributeError:
        raise SyntaxError(f"expected callable, got {proc!r}") from None
    ctx.argl = cons(create_continuation(ctx), EL)
    return proc


@glbl("car")
def op_car(ctx):
    return unary(ctx, car)


@glbl("cdr")
def op_cdr(ctx):
    return unary(ctx, cdr)


@glbl("cons")
def op_cons(ctx):
    return binary(ctx, cons)


@glbl("div")
def op_div(ctx):
    return binary(ctx, op_div_f)


def op_div_f(x, y):
    if isinstance(x, int) and isinstance(y, int):
        return x // y
    return x / y


@glbl("eq?")
def op_eq(ctx):
    return binary(ctx, op_eq_f)


def op_eq_f(x, y):
    return T if eq(x, y) else EL


@glbl("equal?")
def op_equal(ctx):
    return binary(ctx, op_equal_f)


def op_equal_f(x, y):
    return T if x == y else EL


@glbl("error")
def op_error(ctx):
    raise error(ctx.unpack1())


@glbl("eval")
def op_eval(ctx):
    try:
        x = ctx.unpack1()
        n_up = 0
    except SyntaxError:
        x, n_up = ctx.unpack2()
    if x.__class__ is str:
        l = []
        parse(ctx, x, l.append)
        x = l[-1] if l else EL
    e = ctx.env
    for _ in range(n_up):
        e = e[SENTINEL]
        if e is SENTINEL:
            raise SyntaxError("no frame available")
    ctx.exp = x
    ctx.env = e
    return k_leval


@glbl("exit")
def op_exit(ctx):
    x = ctx.unpack1()
    if isinstance(x, int):
        raise SystemExit(x)
    ctx.exp = x
    ctx.cont = k_op_exit
    return k_stringify


def k_op_exit(ctx):
    raise SystemExit(ctx.val)


@glbl("last")
def op_last(ctx):
    x = ctx.unpack1()
    try:
        ret = EL
        while x is not EL:
            ret, x = x
    except TypeError:
        raise SyntaxError(f"expected list, got {x!r}") from None
    ctx.val = ret
    return ctx.cont


@glbl("lt?")
@glbl("<")
def op_lt(ctx):
    return binary(ctx, op_lt_f)


def op_lt_f(x, y):
    return T if x < y else EL


@glbl("mul")
@glbl("*")
def op_mul(ctx):
    return binary(ctx, op_mul_f)


def op_mul_f(x, y):
    return x * y


@glbl("nand")
def op_nand(ctx):
    return binary(ctx, op_nand_f)


def op_nand_f(x, y):
    if not (isinstance(x, int) and isinstance(y, int)):
        raise TypeError(f"expected integers, got {x!r} and {y!r}")
    return ~(x & y)


@glbl("null?")
def op_null(ctx):
    x = ctx.unpack1()
    ctx.val = T if x is EL else EL
    return ctx.cont


@glbl("print")
def op_print(ctx):
    args = ctx.argl

    if args is EL:
        print()
        ctx.val = EL
        return ctx.cont

    arg, args = args

    ctx.push(ctx.cont)
    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


def k_op_print(ctx):
    args = ctx.pop()

    if args is EL:
        print(ctx.val)
        ctx.val = EL
        return ctx.pop()

    print(ctx.val, end=" ")

    arg, args = args

    ctx.push(args)
    ctx.exp = arg
    ctx.cont = k_op_print
    return k_stringify


@glbl("set-car!")
def op_setcar(ctx):
    return binary(ctx, set_car)


@glbl("set-cdr!")
def op_setcdr(ctx):
    return binary(ctx, set_cdr)


@glbl("sub")
@glbl("-")
def op_sub(ctx):
    return binary(ctx, op_sub_f)


def op_sub_f(x, y):
    return x - y


@glbl("type")
def op_type(ctx):
    def f(x):
        ## pylint: disable=too-many-return-statements
        if x is EL:
            return ctx.symbol("()")
        if x is T:
            return ctx.symbol("#t")
        if isinstance(x, list):
            return ctx.symbol("pair")
        if isinstance(x, Symbol):
            return ctx.symbol("symbol")
        if isinstance(x, int):
            return ctx.symbol("integer")
        if isinstance(x, float):
            return ctx.symbol("float")
        if isinstance(x, str):
            return ctx.symbol("string")
        if getattr(x, "lambda_", None):
            return ctx.symbol("lambda")
        if getattr(x, "continuation", False):
            return ctx.symbol("continuation")
        if callable(x):
            return ctx.symbol("primitive")
        return ctx.symbol("opaque")

    return unary(ctx, f)


@glbl("while")
def op_while(ctx):
    x = ctx.unpack1()
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    ctx.push(ctx.cont)
    ctx.push(x)
    ctx.push(ctx.env)
    ctx.argl = EL
    ctx.cont = k_op_while
    return x


def k_op_while(ctx):
    ctx.env = ctx.pop()
    x = ctx.top()

    if ctx.val is EL:
        ctx.pop()  ## x
        return ctx.pop()
    ctx.push(ctx.env)
    ctx.argl = EL
    ctx.cont = k_op_while
    return x


## }}}
## {{{ ffi


def module_ffi(args, module):
    if not args:
        raise TypeError("at least one arg required")
    sym = symcheck(args.pop(0))
    func = getattr(module, str(sym), SENTINEL)
    if func is SENTINEL:
        raise ValueError(f"function {sym!r} does not exist")
    return func(*args)


@ffi("math")
def op_ffi_math(args):
    import math  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, math)


@ffi("random")
def op_ffi_random(args):
    import random  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, random)


@ffi("range")
def op_ffi_range(args):
    return list(range(*args))


@ffi("shuffle")
def op_ffi_shuffle(args):
    import random  ## pylint: disable=import-outside-toplevel

    (l,) = args
    random.shuffle(l)
    return l


@ffi("time")
def op_ffi_time(args):
    import time  ## pylint: disable=import-outside-toplevel

    def f(args):
        return [tuple(arg) if isinstance(arg, list) else arg for arg in args]

    return module_ffi(f(args), time)


## }}}
## {{{ lisp runtime


RUNTIME = r"""
;; {{{ quasiquote

(special xquasiquote (lambda (x) (qq- x (lambda (x) (eval x 1)))))

(define qq-queue (lambda () (list () ())))

(define qq-hed (lambda (q) (car q)))

(define qq-enq (lambda (q x) (do
    (define n (cons x ()))
    (if
        (null? (car q))
        (set-car! q n)
        (set-cdr! (cdr q) n)
    )
    (set-cdr! q n)
    (car q)
)))

(define qq-lst (lambda (q l) (do
    (if
        (null? l)
        ()
        (do
            (qq-enq q (car l))
            (qq-lst q (cdr l))
        )
    )
    (car q)
)))

(define qq- (lambda (form evaluator) (do
    (if
        (pair? form)
        (qq-pair form evaluator)
        form
    )
)))

(define qq-pair (lambda (form evaluator) (do
    (define q (qq-queue))
    (if
        (null? (cdr (cdr form)))
        (qq-pair-2 form q evaluator)
        (qq-list form q evaluator)
    )
)))

(define qq-pair-2 (lambda (form q evaluator) (do
    (define app (car form))
    (cond
        ((eq? app 'quasiquote) (qq-enq q (qq- (cadr form) evaluator)))  ; XXX correct?
        ((eq? app 'unquote) (evaluator (cadr form)))
        ((eq? app 'unquote-splicing) (error "cannot do unquote-splicing here"))
        (#t (qq-list form q evaluator))
    )
)))

(define qq-list (lambda (form q evaluator) (do
    (if
        (null? form)
        ()
        (do
            (define elt (car form))
            (if
                (pair? elt)
                (if
                    (null? (cdr (cdr elt)))
                    (if
                        (eq? (car elt) 'unquote-splicing)
                        (qq-lst q (evaluator (cadr elt)))
                        (qq-enq q (qq- elt evaluator))
                    )
                    (qq-enq q (qq- elt evaluator))
                )
                (qq-enq q (qq- elt evaluator))
            )
            (qq-list (cdr form) q evaluator)
        )
    )
    (qq-hed q)
)))
;; }}}
;; {{{ basics

;; to accompany quasiquote
(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

;; used everywhere
(define pair? (lambda (x) (if (eq? (type x) 'pair) #t ())))
(define list  (lambda (& args) args))

;; ditto
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))

;; }}}
;; {{{ begin

(define begin do)

;; }}}
;; {{{ foreach
;; call f for each element of lst

(define foreach (lambda (f lst) ( do
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? lst)
        ()
        ( do
            (f (car lst))
            (set! lst (cdr lst))
            (c c)
        )
    )
)))

;; }}}
;; {{{ list-builder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time. it's an ad-hoc queue

(define list-builder (lambda () ( do
    (define ht (list () ()))
    (define add (lambda (x) ( do
        (define node (cons x ()))
        (if
            (null? (car ht))
            ( do
                (set-car! ht node)
                (set-cdr! ht node)
            )
            ( do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            )
        )
        dispatch
    )))
    (define dispatch (lambda (op & args)
        (if
            (eq? op 'add)
            (if
                (null? (cdr args))
                (add (car args))
                (error "add takes a single arg")
            )
            (if
                (eq? op 'extend)
                (if
                    (null? (cdr args))
                    ( do
                        (foreach add (car args))
                        dispatch
                    )
                    (error "extend takes a single list arg")
                )
                (if
                    (eq? op 'get)
                    (car ht)
                    (error "unknown command")
                )
            )
        )
    ))
    dispatch
)))

;; }}}
;; {{{ def

(define def define)

;; }}}
;; {{{ bitwise ops

;; bitwise ops from nand
(define (bnot x)   (nand x x))
(define (band x y) (bnot (nand x y)))
(define (bor  x y) (nand (bnot x) (bnot y)))
(define (bxor x y) (band (nand x y) (bor x y)))

;; }}}
;; {{{ arithmetic

(define (neg x) (sub 0 x))
(define (add x y) (sub x (neg y)))

;; oh, and mod
(define (mod n d) (sub n (mul d (div n d))))

;; absolute value
(define (abs x)
    (if
        (lt? x 0)
        (neg x)
        x
    )
)

;; copysign
(define (copysign x y)
    (if
        (lt? y 0)
        (neg (abs x))
        (abs x)
    )
)

;; (signed) shifts
(define (lshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (add x x))
        (#t             (lshift (lshift x (sub n 1)) 1))
    )
)

(define (rshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (div x 2))
        (#t             (rshift (rshift x (sub n 1)) 1))
    )
)

;; }}}
;; {{{ comparison predicates

(define (le? x y) (if (lt? x y) #t (if (equal? x y) #t ())))
(define (ge? x y) (not (lt? x y)))
(define (gt? x y) (lt? y x))

;; }}}
;; {{{ and or not

(special and (lambda (& __special_and_args__)
    ((lambda (c)
        (cond
            ((null? __special_and_args__) ())
            ((null? (cdr __special_and_args__))
                (eval (car __special_and_args__)))
            ((eval (car __special_and_args__)) ( do
                (set! __special_and_args__ (cdr __special_and_args__))
                (c c)
            ))
            (#t ())
        )
    ) (call/cc (lambda (cc) cc)) )
))

(special or (lambda (& __special_or_args__)
    ((lambda (c)
        (cond
            ((null? __special_or_args__) ())
            ((eval (car __special_or_args__)) #t)
            (#t ( do
                (set! __special_or_args__ (cdr __special_or_args__))
                (c c)
            ))
        )
    ) (call/cc (lambda (cc) cc)) )
))

(define (not x) (if (eq? x ()) #t ()))

;; }}}
;; {{{ assert

(special assert (lambda (__special_assert_sexpr__)
    (if
        (eval __special_assert_sexpr__)
        ()
        (error (>string __special_assert_sexpr__))
    )
))

;; }}}
;; {{{ reverse

(define (reverse l)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        r
        ( do
            (set! r (cons (car l) r))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ iter and enumerate

(define (iter lst fin)
    (define item ())
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (car lst))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

(define (enumerate lst fin)
    (define index 0)
    (define item fin)
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (list index (car lst)))
                    (set! index (add index 1))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

;; }}}
;; {{{ length

(define (length l)
    (define n 0)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        n
        ( do
            (set! n (add n 1))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ fold, transpose, map
;; sicp p.158-165 with interface tweaks
(define (fold-left f initial sequence)
    (define r initial)
    (foreach (lambda (elt) (set! r (f elt r))) sequence)
    r
)

(define reduce fold-left)  ;; python nomenclature

(define (fold-right f initial sequence)
      (fold-left f initial (reverse sequence)))

(define accumulate fold-right)  ;; sicp nomenclature

;(fold-left  cons () (list 1 4 9))  ;; (9 4 1)    (cons 9 (cons 4 (cons 1 ())))
;(fold-right cons () (list 1 4 9))  ;; (1 4 9)    (cons 1 (cons 4 (cons 9 ())))

(define (map1 f lst)
    (define (g elt r) (cons (f elt) r))
    (fold-right g () lst)
)

(define (accumulate-n f initial sequences)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? (car sequences))
        (reverse r)
        ( do
            (set! r (cons (accumulate f initial (map1 car sequences)) r))
            (set! sequences (map1 cdr sequences))
            (c c)
        )
    )
)

(define (transpose lists) (accumulate-n cons () lists))

(define (map f & lists)
    (define (g tuple) (apply f tuple))
    (map1 g (transpose lists))
)

;; }}}
;; {{{ join

(define (join x y)
    (cond
        ((null? x) y)
        ((null? y) x)
        ((null? (cdr x)) (cons (car x) y))
        (#t (fold-right cons (fold-right cons () y) x))
    )
)

;; }}}
;; {{{ queue

(define (queue)
    (define h ())
    (define t ())

    (define (dispatch op & args)
        (cond
            ((eq? op (quote enqueue))
                (if
                    (equal? (length args ) 1)
                    ( do
                        (define node (cons (car args) ()))
                        (if
                            (null? h)
                            (set! h node)
                            (set-cdr! t node)
                        )
                        (set! t node)
                        ()
                    )
                    (error "enqueue takes one arg")
                )
            )
            ((eq? op (quote dequeue))
                (if
                    (equal? (length args) 0)
                        (if
                            (null? h)
                            (error "queue is empty")
                            ( let (
                                (ret (car h)))
                                (do
                                    (set! h (cdr h))
                                    (if (null? h) (set! t ()) ())
                                    ret
                                )
                            )
                        )
                    (error "dequeue takes no args")
                )
            )
            ((eq? op (quote empty?)) (eq? h ()))
            ((eq? op (quote enqueue-many))
                (if
                    (and (equal? (length args) 1) (pair? (car args)))
                    ( do
                        (foreach enqueue (car args))
                        dispatch
                    )
                    (error "enqueue-many takes one list arg")
                )
            )
            ((eq? op (quote get-all)) h)
        )
    )
    dispatch
)


;; }}}
;; {{{ let

(special let (lambda (__special_let_vdefs__ __special_let_body__)
    (eval (let$ __special_let_vdefs__ __special_let_body__) 1)))

(define (let$ vdefs body)
    (define vdecls (transpose vdefs))
    (define vars (car vdecls))
    (define vals (cadr vdecls))
    `((lambda (,@vars) ,body) ,@vals)
)

;; }}}
;; {{{ let*

(special let* (lambda (__special_lets_vdefs__ __special_lets_body__)
    (eval (let*$ __special_lets_vdefs__ __special_lets_body__) 1)))

(define (let*$ vdefs body)
    (if
        (null? vdefs)
        body
        ( do
            (define kv (car vdefs))
            (set! vdefs (cdr vdefs))
            (define k (car kv))
            (define v (cadr kv))
          `((lambda (,k) ,(let*$ vdefs body)) ,v)
        )
    )
)

;; }}}
;; {{{ letrec
;; i saw this (define x ()) ... (set! x value) on stackoverflow somewhere

(special letrec (lambda (__special_letrec_decls__ __special_letrec_body__)
    (eval (letrec$ __special_letrec_decls__ __special_letrec_body__) 1)))

(define (letrec$ decls & body)
    (define names (map1 car decls))
    (define values (map1 cadr decls))
    (define (declare var) `(define ,var ()))
    (define (initialize var-value) `(set! ,(car var-value) ,(cadr var-value)))
    (define (declare-all) (map1 declare names))
    (define (initialize-all) (map1 initialize decls))
    `((lambda () ( do ,@(declare-all) ,@(initialize-all) ,@body)))
)

;; }}}
;; {{{ associative table

(define (table compare)
    (define items ())
    (define (dispatch m & args)
        (cond
            ((eq? m 'known) (not (null? (table$find items key compare))))
            ((eq? m 'del) (set! items (table$delete items (car args) compare)))
            ((eq? m 'get) ( do
                (let* (
                    (key (car args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ()
                        (cadr node)
                    )
                )
            ))
            ((eq? m 'iter) ( do
                (let ((lst items))
                    (lambda ()
                        (if
                            (null? lst)
                            ()
                            ( do
                                (define ret (car lst))
                                (set! lst (cdr lst))
                                ret
                            )
                        )
                    )
                )
            ))
            ((eq? m 'len) (length items))
            ((eq? m 'raw) items)
            ((eq? m 'set) ( do
                (let* (
                    (key (car args))
                    (value (cadr args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ( do
                            (let* (
                                (node (cons key (cons value ()))))
                                (set! items (cons node items)))
                        )
                        (set-car! (cdr node) value)
                    )
                )
            ))
            (#t (error "unknown method"))
        )
    )
    dispatch
)

(define (table$find items key compare)
    (cond
      ((null? items) ())
      ((compare (car (car items)) key) (car items))
      (#t (table$find (cdr items) key compare))
    )
)

(define (table$delete items key compare)
    (define prev ())
    (define (helper assoc key)
        (cond
            ((null? assoc) items)
            ((compare (car (car assoc)) key) (do
                (cond
                    ((null? prev) (cdr assoc))
                    (#t (do (set-cdr! prev (cdr assoc)) items))
                )
            ))
            (#t ( do
                (set! prev assoc)
                (helper (cdr assoc) key)
            ))
        )
    )
    (helper items key)
)

;; }}}
;; {{{ looping: loop, for

;; call f in a loop forever
(define (loop f)
    (define c (call/cc (lambda (cc) cc)))
    (f)
    (c c)
)

;; call f a given number of times as (f counter)
(define (for f start stop step)
    (if (lt? step 1) (error "step must be positive") ())
    (define i start)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? i stop)
        ( do
            (f i)
            (set! i (add i step))
            (c c)
        )
        ()
    )
)

;; }}}
;; {{{ iterate (compose with itself) a function

(define (iter-func f x0 n)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? n 1)
        x0
        (do
            (set! x0 (f x0))
            (set! n (sub n 1))
            (c c)
        )
    )
)

;; }}}
;; {{{ benchmarking

(define (timeit f n)
    (define t0 (time 'time))
    (for f 0 n 1)
    (define t1 (time 'time))
    (define dt (sub t1 t0))
    (if (lt? dt 1e-7) (set! dt 1e-7) ())
    (if (lt? n 1) (set! n 1) ())
    (list n dt (mul 1e6 (div dt n)) (div n dt))
)

;; }}}
;; {{{ gcd

(define (gcd x y)
    (cond
        ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        (#t ( do
            (define c (call/cc (lambda (cc) cc)))
            (if
                (equal? y 0)
                x
                ( do
                    (define r (mod x y))
                    (set! x y)
                    (set! y r)
                    (c c)
                )
            )
        ))
    )
)

;; }}}
;; {{{ smul

;; signed integer multiplication from subtraction and right shift (division)
(define umul (lambda (x y accum)
    ((lambda (c)
        (if
            (equal? 0 x)
            accum
            ((lambda (& _) (c c))
                (if
                    (equal? (band x 1) 1)
                    (set! accum (add accum y))
                    ()
                )
                (set! x (div x 2))
                (set! y (mul y 2))
            )
        )
    ) (call/cc (lambda (cc) cc)))
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

;; }}}

;; EOF
"""


## }}}


def main():
    ctx = Context()
    parse(ctx, RUNTIME, ctx.leval)
    return lmain(ctx)


if __name__ == "__main__":
    main()


## EOF
