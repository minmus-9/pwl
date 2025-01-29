;; t.lisp - test code
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

(print "t.lisp start")

;; {{{ rational number arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; }}}
;; {{{ kernel concept
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

;; }}}
;; {{{ more kernel concept
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

;; }}}
;; {{{ factorials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fastest for the dict-based KeyedTable case
(define !1 (lambda (n)
    (if
        (define n! 1)
        ()
        ((lambda (c _ _)                ;; huh. gotta love it!
            (if (lt? n 2) n! (c c)))    ;; misleading formatting++
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

;; the winner!
(def (!3 n)  ;; significantly faster than !1 for non-dict KeyedTable case
             ;; marginally slower than !1 for dict-based KeyedTable
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

(def (!5 n)
    (define cont ())
    (define n! 1)
    (define k (call/cc (lambda (cc) (do (set! cont cc) n))))
    (set! n! (mul n! k))
    (cond
        ((lt? n 1) 1)
        ((lt? k 2) n!)
        (#t (cont (sub k 1)))
    )
)

(def (!6 n)
    (def (iter n! k)
        (if
            (lt? k 2)
            n!
            (iter (mul n! k) (sub k 1))
        )
    )
    (iter 1 n)
)

(def (!7 n)
     (math 'factorial n)
)

(def (!8 n)
    (fold-left mul 2 (range 3 (add n 1) 1))
)

(def (xrange start stop step)
    (define i (sub start step))
    (def (next)
        (if
            (ge? i stop)
            ()
            ( do
                (set! i (add i step))
                i
            )
        )
    )
    next
)

(def (!9 n)
    (def (f r)
        (if
            (null? (do (define k ((car r))) k))
            (cdr r)
            (f (cons (car r) (mul (cdr r) k)))
        )
    )
    (f (cons (xrange 2 n 1) 1))
)

(def (!10 n)
    (let* (
        (it (xrange 2 n 1))
        (c  ())
        (n! 1)
        (k  (call/cc (lambda (cc) (do (set! c cc) (it))))))
        (if
            (null? k)
            n!
            (do (set! n! (mul n! k)) (c (it)))
        )
    )
)

(def (!11 n)
    (define c ())
    ((lambda (n! k) ( do
        (set! n (sub k 1))
        (if (lt? k 2) n! (c (mul n! k)))))
        (call/cc (lambda (cc) (do (set! c cc) 1)))
        n
    )
)

(def (!12 n)
    (define c ())
    (def (f n!k)
        (if
            (lt? (cdr n!k) 2)
            (car n!k)
            (c
                (cons
                    (mul (car n!k) (cdr n!k))
                    (sub (cdr n!k) 1)
                )
            )
        )
    )
    (f
        (call/cc
            (lambda (cc) (do
                (set! c cc)
                (cons 1 n))
            )
        )
    )
)

(def (!13 n)
    (def (f info)
        (if
            (lt? (cadr info) 2)
            (car info)
            ((caddr info)
                (list
                    (mul (car info) (cadr info))
                    (sub (cadr info) 1)
                    (caddr info)
                )
            )
        )
    )
    (f (call/cc (lambda (cc) (list 1 n cc))))
)

(def (!14 n)
    (def (f x)
        (set! n (sub n 1))
        (mul n x)
    )
    (iter-func f n (sub n 1))
)

(def (!15 n)
    (def (f nn!)
        (define n (car nn!))
        (define n! (cdr nn!))
        (cons
            (add n 1)
            (mul n n!)
        )
    )
    (cdr (iter-func f (cons 1 1) n))
)

;; time 5 reps of 100!, record time per rep in ms
;;
;; calibration time listed as "nil" has been
;; subtracted from the 1-13 numbers
;;
;;  2025-01-28
;;  commmit 009e665e962cec1477847b91dd82294f54f3b1e1
;;
;;        home        home       galaxy
;;        fast        slow        s24+
;;       =====       =====       =====
;; nil       1         2.5         1.4
;;   1       9          22          12
;;   2      25          54          30
;;   3      10          21          12
;;   4     123         271         153
;;   5      70         159          89
;;   6      26          58          33
;;   7      <1          <1          <1
;;   8      15          35          19
;;   9     240         543         303
;;  10     195         444         249
;;  11      10          24          13
;;  12      15          36          19
;;  13      31          72          41
;;  14      41          92         ***
;;  15     147         339         ***
;;
;; starting 95406b3b3f54d14bb60168f573bfe2307a362f71
;; just record runtime of (!bench), same 15 impls
;;
;;    home        home       galaxy
;;    fast        slow        s24+
;;   =====       =====       =====
;;    4.76       10.75       *****      95406b3b3f54d14bb60168f573bfe2307a362f71
;;    4.61       10.43       *****      3d9e7d5ff52d7598cc4b25fa6178e3435beab9ed
;;
(def (!bench)
    (define reps 5)
    (define n 100)
    (print 'nil (timeit (lambda (_) ()) 10))
    (print '!1  (timeit (lambda (_) (!1 n)) reps))
    (print '!2  (timeit (lambda (_) (!2 n)) reps))
    (print '!3  (timeit (lambda (_) (!3 n)) reps))
    (print '!4  (timeit (lambda (_) (!4 n)) reps))
    (print '!5  (timeit (lambda (_) (!5 n)) reps))
    (print '!6  (timeit (lambda (_) (!6 n)) reps))
    (print '!7  (timeit (lambda (_) (!7 n)) reps))
    (print '!8  (timeit (lambda (_) (!8 n)) reps))
    (print '!9  (timeit (lambda (_) (!9 n)) reps))
    (print '!10 (timeit (lambda (_) (!10 n)) reps))
    (print '!11 (timeit (lambda (_) (!11 n)) reps))
    (print '!12 (timeit (lambda (_) (!12 n)) reps))
    (print '!13 (timeit (lambda (_) (!13 n)) reps))
    (print '!14 (timeit (lambda (_) (!14 n)) reps))
    (print '!15 (timeit (lambda (_) (!15 n)) reps))
)
(timeit (lambda (_) (!bench)) 1)

;; }}}

(print '************************************************************)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "t.lisp done")
