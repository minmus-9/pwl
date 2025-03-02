;; factorial.lisp - yup
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

;; NB this only works with lisp04-trampolined-fancy/lisp.py lisp/lisp04.lisp

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
     (for f 2 (add n 1) 1)
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

(def (!16 n)
    (define n! 1)
    ((lambda (c & _)
        (if (lt? n 2) n! (c c)))
        (call/cc (lambda (cc) cc))
        (set! n! (mul n! n))
        (set! n  (sub n  1))
    )
)

(def (!17 n)
    (define l ())
    (define n! 1)
    (for
        (lambda (k) (set! l (cons k l)))
        2
        (add n 1)
        1
    )
    (while (lambda ()
        (if
            (null? l)
            ()
            (do
                (set! n! (mul n! (car l)))
                (set! l (cdr l))
                #t
            )
        )
    ))
    n!
)

(def (!18 n)
    (cond
        ((lt? n 2) 1)
        ((lt? n 3) 2)
        ((lt? n 4) 6)
        ((lt? n 5) 24)
        (#t (mul n (!18 (sub n 1))))
    )
)

(def (!19 n)
    ((lambda (f) (f f 1 n))
        (lambda (f p k)
            (if (lt? k 2)
                p
                (f f (mul p k) (sub k 1))
            )
        )
    )
)

(def (!bench)
    (define reps 5)
    (define n 400)
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
    (print '!16 (timeit (lambda (_) (!16 n)) reps))
    (print '!17 (timeit (lambda (_) (!17 n)) reps))
    (print '!18 (timeit (lambda (_) (!18 n)) reps))
    (print '!19 (timeit (lambda (_) (!19 n)) reps))
)
(timeit (lambda (_) (!bench)) 1)
