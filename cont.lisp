;; cont.lisp - continuation-based runtime for lisp.py and up
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
;; looping

;; call f in a loop forever
(define loop (lambda (f) ( do
    (define c (call/cc (lambda (cc) cc)))
    (f)
    (c c)
)))

;; loop while f returns true
(define while (lambda (f) ( do
    (define c ())
    (define flag (call/cc (lambda (cc) (do (set! c cc) #t))))
    (cond
        (flag (c (f)))
        (#t ())
    )
)))

;; loop until f returns true
(define until (lambda (f) ( do
    (define c ())
    (define flag (call/cc (lambda (cc) (do (set! c cc) ()))))
    (cond
        (flag ())
        (#t (c (f)))
    )
)))

;; call f for each element of lst
;; this is faster than the stdlib version
(define foreach (lambda (f lst) ( do
    (define c (call/cc (lambda (cc) cc)))
    (cond
        ((null? lst) ())
        (#t ( do
            (f (car lst))
            (set! lst (cdr lst))
            (c c)
        ))
    )
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; this is faster than the ffi one
(define reverse (lambda (lst) ( do
    (define r ())
    (define f (lambda ()
        (cond
            ((null? lst) ())
            (#t (do
                (set! r (cons (car lst) r))
                (set! lst (cdr lst))
                #t
            ))
        )
    ))
    (while f)
    r
)))

;; EOF
