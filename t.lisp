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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continuation based factorial, slower than sicp iterative solution

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

(print (! 100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational number arithmetic

(define gcd (lambda (x y)
    (cond
        ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        ((equal? y 0) x)
        (#t (gcd y (mod x y)))
    )
))

(timeit (lambda (_) (gcd 2379728399026437315402 2173491264856982165498)) 4)

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
        ((pair? z) (kexec z))
        (#t z)
    )
)))

(define kexec (lambda (args)
    (let* (
        (m (car args))
        (c (cadr args))
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
            ((pair? kc) (kexec kc))
            (#t kc)
        )
    )))
    (define kexec (lambda (args) (let* (
        (m (car args))
        (c (cadr args))
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
    (lambda (k c args) (c (time 'time))))
(k 'register 'test
    (lambda (k c args) (c (reverse args))))

(k 'call 'time)
(k 'call 'test 'a 'b 'c)


;; {{{ factorials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define !1 (lambda (n)
    (if
        (define n! 1)
        ()
        ((lambda (c _ _)                ;; huh. gotta love it!
            (if (lt? n 1) n! (c c)))
            (call/cc (lambda (cc) cc))
            (set! n! (mul n! n))
            (set! n (sub n 1))
        )
    )
))

(def (!2 n)
    (if
        (lt? n 2)
        1
        (mul n (!2 (sub n 1)))
    )
)

(def (!3 n)
    (define n! 1)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? n 2)
        n!
        ( do
            (set! n! (mul n n!))
            (set! n  (sub n 1))
            (c c)
        )
    )
)

(def (!4 n)
     (define n! 1)
     (def (f k) (set! n! (mul n! k)))
     (for f 1 n 1)
     n!
)

(timeit (lambda (_) ()) 10)
;; for 100!
;; !1  48ms
;; !2 115ms
;; !3  48ms
;; !4 736ms
(timeit (lambda (_) (!1 40)) 10)
(timeit (lambda (_) (!2 40)) 10)
(timeit (lambda (_) (!3 40)) 10)
(timeit (lambda (_) (!4 40)) 10)

;; }}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "t.lisp done")
