;; t - test code
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; this file is just for fiddling around

;; for lwp.py
;(define list? (lambda (x) (pair? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playing with quasiquote impl

(special qq (lambda (form) (qq$guts form)))

(define qq$guts (lambda (form)
    (cond
        ((eq? form ()) form)
        ((list? form) ((qq$list (list-builder) form) (quote get)))
        (#t form)
    )
))

(define qq$elt (lambda (lb elt)
    (cond
        ((eq? elt ()) (lb (quote add) elt))
        ((list? elt) (qq$list lb elt))
        (#t (lb (quote add) elt))
    )
))

(define qq$list (lambda (lb form) ( do
    (define verb (car form))
    (cond
        ((equal? (length form) 2) (
            cond
                ((eq? verb (quote qq)) (lb (quote add) (qq$guts (cadr form))))
                ((eq? verb (quote uq)) (lb (quote add) (eval (cadr form))))
                ((eq? verb (quote ux)) (lb (quote extend) (eval (cadr form))))
                (#t (lb (quote add) form))
        ))
        ((or (eq? verb (quote qq))
             (eq? verb (quote uq))
             (eq? verb (quote ux)))
            (error "qq uq ux take a single arg"))
        (#t (lb (quote add) ((fold-left qq$elt (list-builder) form) (quote get))))
    )
)))

(define uq (lambda (x) (error "can only uq in qq")))
(define ux (lambda (x) (error "can only ux in qq")))

;; this isn't quite working right, i get (correct result) in its own list
(define y (quote (17 31))) (define x 11) (print (qq (add (sub 0 (uq x)) (ux y) 2)))
;(print `(add (sub 0 ,x) ,@y 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continuation based factorial

(define ! (lambda (n) ( do
    (define cont ())
    (define n! 1)
    (define k (call/cc (lambda (cc) (do (set! cont cc) n))))
    (set! n! (mul n! k))
    (cond
        ((equal? n! 0) 1)  ;; 0! is usally treated as 1
        ((lt? k 2) n!)
        (#t (cont (sub k 1)))
    )
)))
;(print (! 10000))
(print (! 100))  ;; got sick of waiting every time i run this


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational number arithmetic

(define gcd (lambda (x y)
    (cond
        ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        ((equal? y 0) x)
        (#t ( do
            (define z (mod x y))
            (gcd y z)
        ))
    )
))

(define rat (lambda (x y) ( do
    (define z (gcd x y))
    (set! x (div x z))
    (set! y (div y z))
    (cond ((lt? y 0) (do
        (set! x (neg x))
        (set! y (neg y)))
    ))
    (cond ((equal? x 0) (set! y 1)))
    (define dispatch (lambda (op & args) ( do
        (cond
            ((eq? op 'num) x)
            ((eq? op 'den) y)
            ((eq? op 'get) (list x y))

            ((eq? op 'neg) (rat (neg x) y))
            ((eq? op 'float) (div (>float x) y))
            ((eq? op 'recip) (rat y x))

            ((eq? op 'add) (do
                (define other (car args))
                (let
                    ((n1 (dispatch 'num))
                     (n2 (other 'num))
                     (d1 (dispatch 'den))
                     (d2 (other 'den)))
                    (let    ;; XXX need to write letrec
                        ((num (add (mul n1 d2) (mul n2 d1)))
                         (den (mul d1 d2)))
                      (rat num den)
                    )
                )
            ))
            ((eq? op 'mul) (do
                (define other (car args))
                (let
                    ((n1 (dispatch 'num))
                     (n2 (other 'num))
                     (d1 (dispatch 'den))
                     (d2 (other 'den)))
                    (rat (mul n1 n2) (mul d1 d2))
                )
            ))

            ((eq? op 'div) (dispatch 'mul ((car args) 'recip)))
            ((eq? op 'sub) (dispatch 'add ((car args) 'neg)))
        )
    )))
    dispatch
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fiddling getting quasiquote to work

(define v 'a) (define e 97) (define l '(1 2))
(define w ())
(define z (lambda (v e) ( do
  (print 'QQ `(set! ,v (add 1 ,e ,@l ,@(list 3 4))))
      (eval  `(set! ,v (add 1 ,e ,@l ,@(list 3 4))))
      (print 'W w)

  (print 'QQ `(set! ,v (add 1 ,e ,@l ,@(list) 17)))
      (eval  `(set! ,v (add 1 ,e ,@l ,@(list) 17)))
)))
(print 'W w)
(z 'w 121)
(print 'W w)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; looks weird but isn't

(define countdown (lambda (n) ( do
    ((lambda (c k) ( do
        (cond
            ((lt? n 1) "OK")
            (#t (do (print 'N n) (set! n k) (c c)))
        )
    ))
        (call/cc (lambda (cc) cc))
        (sub n 3)
    )
)))

(countdown 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "kadd" service executes "kernel" coroutine to handle the blocking
;; kadd request (blocks from the perspective of the (kadd) caller);
;; the "kernel" can fulfill the requests whenever it suits and then
;; wake up the (kadd) caller once the result is ready.

(define kadd (lambda (& args)
    (kcall 'add args)
))

(define kcall (lambda (m args)
    (call/cc (lambda (cc) ((kentry) (join (list m cc) args))))
))

(define kentry (lambda () ( do
    (define z (call/cc (lambda (cc) cc)))
    (cond
        ((list? z) (kexec z))
        (#t z)
    )
)))

(define kexec (lambda (args)
    (let* (
        (m (first args))
        (c (second args))
        (f (kftab 'get m))
        (args (cdr (cdr args))))
      (cond
        ((null? f) (error "unknown function call"))
        (#t (f c args))
      )
    )
))

(define kftab (table eq?))

(kftab 'set 'add
    (lambda (c args) (c (eval (cons add args)))))

(kadd 11 31)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slower recursive version, need tail call handling!
;(define for (lambda (f n) ( do
;    (define helper (lambda (f i n)
;        (cond
;          ((equal? n 0) ())
;          (#t (do (f i) (helper f (add i 1) (sub n 1))))
;        )
;    ))
;    (helper f 0 n)
;)))

(define timeit (lambda (f n) ( do
    (define __define_timeit_t0__ (time.time))
    (for f n)
    (define t1 (time.time))
    (define dt (sub t1 __define_timeit_t0__))
    (list n dt (mul 1e6 (div dt n)) (div n dt))
)))

;(timeit (lambda (_) ()) 10)
;(timeit (lambda (_) (kadd 11 31)) 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define kernel (lambda () ( do
    (define registry (table eq?))
    (define queues (table eq?))
    (define kcall (lambda (m args)
        (call/cc (lambda (cc) ((kentry) (join (list m cc) args))))
    ))
    (define kentry (lambda () ( do
        (define kc (call/cc (lambda (cc) cc)))
        (cond
            ((list? kc) (kexec kc))
            (#t kc)
        )
    )))
    (define kexec (lambda (args) (let* (
        (m (first args))
        (c (second args))
        (a (cdr (cdr args)))
        (f (registry 'get m)))
        (cond
            ((null? f) (error "unknown function"))
            (#t (f dispatch c a))
        )
    )))
    (define getq (lambda (qid) ( do
        (if (not (queues 'known qid)) (queues 'set qid (queue)) ())
        (queues 'get qid)
    )))
    (define dispatch (lambda (m & args)
        (cond
            ((eq? m 'call) (kcall (car args) (cdr args)))
            ((eq? m 'dequeue) ((queues 'get (car args)) 'dequeue))
            ((eq? m 'enqueue) ((queues 'get (car args)) 'enqueue (cadr args)))
            ((eq? m 'create-queue) (getq (car args)))
            ((eq? m 'register) (let (
                (service (car args))
                (implementation (cadr args)))
                (registry 'set service implementation)
            ))
            ((eq? m 'queues) queues)
            ((eq? m 'registry) registry)
        )
    ))
    dispatch
)))

(define k (kernel))
(k 'register 'time
    (lambda (k c args) (c (time.time))))
(k 'register 'test
    (lambda (k c args) (c (reverse args))))

(k 'call 'test 'a 'b 'c)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "t.lisp done")
