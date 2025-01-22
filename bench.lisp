;; bench.lisp - stdlib-based code to benchmark lisps
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

(define one (lambda ()
    (smul
        92837459838576324768578325623487965894695739794823743
        92837459838576324768578325623487965894695739794823743
    )
))

(define two (lambda ()
    (let
        ((x (one))
         (y (one)))
        x
    )
))

(define three (lambda (n l) ( do
    (cond
        ((lt? n 1) l)
        (#t (do
            (three (sub n 1) (cons n l))
        ))
    )
)))

;; some impls override these defs. make sure we're consistent.

(define join (lambda (x y)
    (cond
        ((null? x) y)
        (#t         (cons (car x) (join (cdr x) y)))
    )
))

(define list (lambda (& args) args))

(define rev (lambda (l) (
    cond
        ((null? l)          ())
        (#t                 (join (rev (cdr l)) (list (car l))))
)))

(define four (lambda (n) ( do
    (two) (two) (two) (two) (two) (two) (two) (two) 
    (rev (three n ()))
    (rev (three n ()))
    (rev (three n ()))
    (rev (three n ()))
)))

(define five (lambda () (four 100)))

(five)

;; EOF
