;; lwp.lisp - runtime, much of which is from other sources
;;
;; pwl - python with lisp, a collection of lisp evaluators for Python
;;       https://github.com/minmus-9/pwl
;; Copyright (C) 2025  Mark Hays (github:minmus-9)
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

(define null? (lambda (x) (if (eq? x ()) #t ())))

(special begin$2 (lambda (__special_begin$2_a__ __special_begin$2_b__)
     (if
        (eval __special_begin$2_a__ 1)
        (eval __special_begin$2_b__ 1)
        (eval __special_begin$2_b__ 1)
    )
))

(special do (lambda (& __special_do_args__)
    (eval (do$ __special_do_args__) 1)))

(define do$ (lambda (__special_do$_args__)
    (if
        (null? __special_do$_args__)
        ()
        (if
            (null? (cdr __special_do$_args__))
            (car __special_do$_args__)
            `(begin$2
                ,(car __special_do$_args__)
                ,(do$ (cdr __special_do$_args__)))
        )
    )
))

(special cond (lambda (& __special_cond_pcs__)
    (eval (cond$ __special_cond_pcs__) 1)))

(define cond$ (lambda (__special_cond_pcs__)
    (if
        (null? __special_cond_pcs__)
        ()
        `(if
            ,(car  (car __special_cond_pcs__))
            ,(cadr (car __special_cond_pcs__))
            ,(cond$ (cdr __special_cond_pcs__)))
    )
))

;; define negation and addition first
(define neg     (lambda (x) (sub 0 x)))
(define add2    (lambda (x y) (sub x (neg y))))

;; oh, and mod
(define mod     (lambda (n d) (sub n (mul2 d (div n d)))))

(define add (lambda (x & args)
    (if
        (null? args)
        x
        (eval (join (list (quote add) (add2 x (car args))) (cdr args)))
    )
))

;; absolute value next
(define abs     (lambda (x) (
    cond
        ((lt? x 0)  (neg x))
        (#t         x)
)))

;; this is redefined in the math ffi interface... for pylisp.py
(define copysign (lambda (x y) (
    cond
        ((lt? y 0) (neg (abs x)))
        (#t (abs x))
)))

;; some comparison predicates
(define le? (lambda (x y) (or (lt? x y) (equal? x y))))
(define ge? (lambda (x y) (not (lt? x y))))
(define gt? (lambda (x y) (not (le? x y))))

;; bitwise ops from nand
(define bnot    (lambda (x) (nand x x)))
(define band    (lambda (x y) (bnot (nand x y))))
(define bor     (lambda (x y) (nand (bnot x) (bnot y))))
(define bxor    (lambda (x y) (band (nand x y) (bor x y))))

;; (signed) shifts
(define lshift  (lambda (x n) (
    cond
        ((equal? n 0)   x)
        ((equal? n 1)   (add x x))
        (#t             (lshift (lshift x (sub n 1)) 1))
)))

(define rshift  (lambda (x n) (
    cond
        ((equal? n 0)   x)
        ((equal? n 1)   (div x 2))
        (#t             (rshift (rshift x (sub n 1)) 1))
)))

(define list    (lambda (& args) args))

(define bool (lambda (x) (cond (x #t) (#t ()))))

(special and (lambda (& __special_and_args__) (
    cond
        ((null? __special_and_args__) ())
        ((null? (cdr __special_and_args__))
            (eval (car __special_and_args__)))
        ((eval (car __special_and_args__)) 
             (eval (join (quote (and)) (cdr __special_and_args__))))
        (#t ())
)))

(special or (lambda (& __special_or_args__) (
    cond
        ((null? __special_or_args__) ())
        ((eval (car __special_or_args__))  #t)
        (#t (eval (join (quote (or)) (cdr __special_or_args__))))
)))

(define not     (lambda (x) (cond (x ()) (#t #t))))

(define join (lambda (x y)
    (cond
        ((null? x)  y)
        (#t         (cons (car x) (join (cdr x) y)))
    )
))

(define last    (lambda (l) (
    cond
        ((null? l)          (error "list empty!"))
        ((null? (cdr l))    (car l))
        (#t                 (last (cdr l)))
)))

(define do (lambda (& args) (last args)))

(define reverse (lambda (l) (
    cond
        ((null? l)          ())
        (#t                 (join (reverse (cdr l)) (list (car l))))
)))

(define length (lambda (l) (do
        (define helper (lambda (l n) (
            cond
                ((null? l)  n)
                (#t         (helper (cdr l) (add n 1)))
        )))

        (helper l 0)
)))

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
        (eval __special_let_doit_lam__)
    )))
    (__special_let_decls__)
)))

(special assert (lambda (__special_assert_sexpr__) (
    cond
        ((eval __special_assert_sexpr__)   ())
        (#t (error (>string __special_assert_sexpr__)))
)))

;; signed integer multiplication from subtraction and right shift (division)
(define smul (lambda (x y) (do
    (define umul (lambda (x y z) (
        cond
            ((equal? y 1) x) ;; y could have been -1 on entry to smul
            ((equal? 0 x) z)
            ((equal? 0 (band x 0x1)) (umul (rshift x 1) (add y y) z))
            (#t (umul (rshift x 1) (add y y) (add z y)))
    )))
    (cond
        ((equal? x 0) 0)
        ((equal? y 0) 0)
        ((lt? x 0) (neg (smul (neg x) y)))
        ((equal? x 1) y)
        ((equal? y 1) x)
        (#t (copysign (umul x (abs y) 0) y))
    )
)))

(define first car)
(define cadr (lambda (l) (car (cdr l))))
(define second cadr)
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define third caddr)
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define fourth cadddr)


;; sicp p.158-165
(define accumulate (lambda (f initial sequence) (
    if
        (null? sequence)
        initial
        (f (car sequence) (accumulate f initial (cdr sequence)))
)))

(define fold-left (lambda (op initial sequence) ( do
    (define iter (lambda (result rest) (
        if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))
    )))
    (iter initial sequence)
)))

(define foreach (lambda (f l) (
    cond
        ((null? l) ())
        (#t (do
            (f (car l))
            (foreach f (cdr l))
        ))
)))

(define map1 (lambda (f lst) (
    cond
        ((null? lst) ())
        (#t (cons (f (car lst)) (map1 f (cdr lst))))
)))

(define accumulate-n (lambda (f initial sequences) (
    if
        (null? (car sequences)) ;; assume other seqs are empty too
        ()
        (cons
            (accumulate f initial (map1 car sequences))
            (accumulate-n f initial (map1 cdr sequences))
        )
)))

(define transpose (lambda (lists) (
    accumulate-n cons () lists
)))

(define map (lambda (f & lists) (do
    (define g (lambda (tuple) (eval (join (quote (f)) tuple))))
    (map1 g (transpose lists))
)))

;;; EOF
