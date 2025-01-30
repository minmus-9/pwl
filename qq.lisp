;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playing with quasiquote impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(special begin$2 (lambda (__special_begin$2_a__ __special_begin$2_b__)
     (if  ;; can't remember where i saw this but i like it
        (eval __special_begin$2_a__ 1)
        (eval __special_begin$2_b__ 1)
        (eval __special_begin$2_b__ 1)
    )
))

(define list (lambda (& args) args))
(define noop (lambda (& args) ()))

;; call f for each element of lst
(define foreach (lambda (f lst)
    (if
        (define c (call/cc (lambda (cc) cc)))
        ()
        (if
            (eq? lst ())
            ()
            (if
                (noop (f (car lst)))
                ()
                (if
                    (set! lst (cdr lst))
                    ()
                    (c c)
                )
            )
        )
    )
))

(define list-builder (lambda () ( begin$2
    (define ht (list () ()))

    (begin$2
        (define add (lambda (x) ( begin$2
            (if
                (define node (cons x ()))
                ()
                (if
                    (eq? (car ht) ())
                    (if
                        (set-car! ht node)
                        ()
                        (set-cdr! ht node)
                    )
                    (if
                        (set-cdr! (cdr ht) node)
                        ()
                        (set-cdr! ht node)
                    )
                )
            )
            ()
        )))
        (begin$2
            (define dispatch (lambda (op & args)
                (if
                    (eq? op 'add)
                    (if
                        (eq? (cdr args) ())
                        (begin$2 (add (car args)) dispatch)
                        (error "add takes a single arg")
                    )
                    (if
                        (eq? op 'extend)
                        (if
                            (eq? (cdr args) ())
                            (if
                                (foreach add (car args))
                                ()
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
        )
    )
)))


(print "start qq.lisp")

(define pair? (lambda (form) (if (eq? (type form) 'pair) #t ())))
(define cadr (lambda (x) (car (cdr x))))

(special qq (lambda (form) (qq$body form)))

(define qq$body (lambda (form)
    (if
        (pair? form)
        (car ((qq$list form (list-builder)) 'get))
        form
    )
))

(define qq$guts (lambda (form)
    (if
        (pair? form)
        ((qq$list form (list-builder)) 'get)
        form
    )
))

(define qq$elt (lambda (elt lb)
    (if
        (pair? elt)
        (qq$list elt lb)
        (lb 'add elt)
    )
))

(define qq$list (lambda (form lb)
    (if
        (define verb (car form))
        ()
        (if
            (eq? (cdr (cdr form)) ())
            (if
                (eq? verb 'quasiquote)
                (lb 'add (qq$guts (cadr form)))
                (if
                    (eq? verb 'unquote)
                    (lb 'add (eval (cadr form)))
                    (if
                        (eq? verb 'unquote-splicing)
                        (lb 'extend (eval (cadr form)))
                        (lb 'add form)
                    )
                )
            )
            (if
                (eq? verb 'quasiquote)
                (error "quasiquote unquote unquote-splicing take a single arg")
                (if
                    (eq? verb 'unquote)
                    (error "quasiquote unquote unquote-splicing take a single arg")
                    (if
                        (eq? verb 'unquote-splicing)
                        (error "quasiquote unquote unquote-splicing take a single arg")
                        (if
                            (define lb2 (list-builder))
                            ()
                            (if
                                (define f (lambda (elt) (qq$elt elt lb2)))
                                ()
                                (if
                                    (foreach f form)
                                    ()
                                    (lb 'add (lb2 'get))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
))

(define y (quote (17 31))) (define x 11)
(print (qq (add (sub 0 (unquote x)) (unquote-splicing y) 2)))
(print (qq (unquote x)))
(print (qq x))  ;; blows up with (car) in qq defn

(print `(add (sub 0 ,x) ,@y 2))

(print "end qq.lisp")

