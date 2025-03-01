;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate pi
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

(def (pi1 _)
    (define z 1)
    (define k 3)
    (define s -1.0)
    (def (f c & _) (if (lt? k 25000) (c c) ()));print (mul z 4))))
    (f  (call/cc (lambda (cc) cc))
        (set! z (add z (div s k)))
        (set! k (add k 2))
        (set! s (neg s))
    )
)
(timeit pi1 1)

(def (pi2 & _)
    (define k 2)
    (define a 4)
    (define b 1)
    (define a1 12)
    (define b1 4)
    (define d ())
    (define d1 ())
    (def (next)
        (define p (mul k k))
        (define q (add (mul k 2) 1))
        (set! k (add k 1))
        (define t1 (add (mul p a) (mul q a1)))
        (define t2 (add (mul p b) (mul q b1)))
        (set! a a1)
        (set! b b1)
        (set! a1 t1)
        (set! b1 t2)
        (set! d (div a b))
        (set! d1 (div a1 b1))
        (while inner)
        (if
            (lt? k 20)
            #t
            ()
        )
    )
    (def (inner)
        (if
            (equal? d d1)
            (do
                ;(print d)
                (set! a  (mul 10 (mod a b)))
                (set! a1 (mul 10 (mod a1 b1)))
                (set! d  (div a b))
                (set! d1 (div a1 b1))
                #t
            )
            ()
        )
    )
    (while next)
)
(timeit pi2 100)
