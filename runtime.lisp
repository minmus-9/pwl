;; runtime that builds on stdlib.lisp
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

;; this code won't work with easy.py which is why this is a separate file.
;; it will work for rec.py and up

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time

(define list-builder (lambda () ( do
    (define ht (list () ()))

    (define get (lambda () (car ht)))

    (define add (lambda (x) ( do
        (define node (cons x ()))
        (cond
            ((null? (car ht)) ( do
                (set-car! ht node)
                (set-cdr! ht node)
            ))
            (#t (do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            ))
        )
        dispatch
    )))

    (define dispatch (lambda (op & args)
        (cond
            ((eq? op (quote add))
                (cond
                    ((equal? (length args) 1) (add (car args)))
                    (#t (error "add takes a single arg"))
                )
            )
            ((eq? op (quote extend))
                (cond
                    ((equal? (length args) 1) ( do
                        (foreach add (car args))
                        dispatch
                    ))
                    (#t (error "extend takes a single list arg"))
                )
            )
            ((eq? op (quote get)) (car ht))
        )
    ))

    dispatch
)))

;; the stdlib reverse routine is quadratic time in list size; this one is
;; linear. but we can't put it into stdlib because lb fails for easy.py.
;; and i want easy.py to be able to use stdlib.

;; for rec.py and up, let's take the linear version...

(define reverse (lambda (lst) (do
    (define LB (list-builder))
    (accumulate
        (lambda (x lb) (lb (quote add) x))
        LB
        lst
    )
    (LB (quote get))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quasiquote - this completely blew my mind. see:
;;;
;;;     https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html

(special quasiquote (lambda (form)
    (quasiquote- form)
))

(define quasiquote- (lambda (form) ( do
    (cond
        ((and (list? form) (gt? (length form) 0))
            (quasiquote-list form))
        (#t form)
    )
)))

(define quasiquote-list (lambda (form) (do
    (define app (car form))

    ;; you want to use list-builder (see lb.lisp) here because
    ;; fold-left generates elements in order. you *need* them
    ;; evaluated in order, too! which means you can't just cons
    ;; things together
    (define z (lambda (lb elem) (
        if
            (and
                (list? elem)
                (equal? (length elem) 2)
                (eq? (car elem) (quote unquote-splicing)))
                (lb (quote extend) (eval (cadr elem)))
                (lb (quote add) (quasiquote- elem))
    )))

    (cond
        ((and (eq? app (quote quasiquote))
              (equal? (length form) 2))
           (quasiquote- (cadr form)))
        ((eq? app (quote quasiquote)) (error "quasiquote takes a single arg"))

        ((and (eq? app (quote unquote))
              (equal? (length form) 2))
           (eval (cadr form)))
        ((eq? app (quote unquote)) (error "unquote takes a single arg"))

        ((and (eq? app (quote unquote-splicing))
              (equal? (length form) 2))
           (error "unquote-splicing must appear in list context"))
        ((eq? app (quote unquote-splicing))
             (error "unquote-splicing takes a single arg"))

        (#t ((fold-left z (list-builder) form) (quote get)))
    )
)))

(define unquote (lambda (x) (error "can only unquote inside quasiquote")))

(define unquote-splicing (lambda (x) (
    error "can only unquote-splicing inside quasiquote"
)))

;; save some (eval (join (quote (sym)) args)) awkwardness
(special eval-flattened (lambda (sym & args) ( do
    (define lb (list-builder))
    (lb (quote add) sym)
    (define f (lambda (lst) ( do
        (define x (eval lst))
        (cond
            ((list? x) (lb (quote extend) x))
            (#t (lb (quote add) x))
        )
    )))
    (foreach f args)
    (eval (lb (quote get)))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; it's still python-thinking today :D

(define iter (lambda (lst fin) (do
    (define item ())
    (define next (lambda ()
        (cond
            ((null? lst) fin)
            (#t (do
                    (set! item (car lst))
                    (set! lst (cdr lst))
                    item
                )
            )
        )
    ))
    next
)))

(define enumerate (lambda (lst fin) (do
    (define index 0)
    (define item fin)
    (define next (lambda ()
        (cond
            ((null? lst) fin)
            (#t (do
                    (set! item (list index (car lst)))
                    (set! index (add index 1))
                    (set! lst (cdr lst))
                    item
                )
            )
        )
    ))
    next
)))
