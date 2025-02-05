;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pylisp.lisp - wrappers for pylisp.py ffi exports
;;
;; only works with pylisp, but only because pylisp defines this
;; ffi interface. said another way, if the ffi code were draggged
;; into rec.py and all the ticks in here were changed to (quote),
;; this would work.
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
;; misc
;;
(define >float (lambda (x) (ffi '>float x)))
(define >int (lambda (x) (ffi '>int x)))
(define >string (lambda (x) (ffi '>string x)))
(define >symbol (lambda (x) (ffi '>symbol x)))

(define chr (lambda (x) (ffi 'chr x)))
(define ord (lambda (x) (ffi 'ord x)))

(define pow (lambda (x n & args) (
    eval (join (quote (ffi 'pow x n)) args)
)))
(define range (lambda (& args) (
    eval (join (quote (ffi 'range)) args)
)))
(define round (lambda (x & args) (
    eval (join (quote (ffi 'round x)) args)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists

(define list.append (lambda (l x) (ffi 'list.append l x)))
(define list.concat (lambda (& args) (ffi 'list.concat args)))
(define list.index (lambda (l x) (ffi 'list.index l x)))
(define list.length (lambda (l) (ffi 'list.length l)))
(define list.mul (lambda (l n) (ffi 'list.mul l n)))
(define list.pop (lambda (l) (ffi 'list.pop l)))
(define list.reverse (lambda (l) (ffi 'list.reverse l)))
(define list.sort (lambda (l) (ffi 'list.sort l)))
(define list.splice (lambda (l bgn end x) (ffi 'list.splice l bgn end x)))
(define list.sublist (lambda (l bgn end) (ffi 'list.sublist l bgn end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math

(define math.e (lambda () (ffi 'math 'e)))
(define math.inf (lambda () (ffi 'math 'inf)))
(define math.nan (lambda () (ffi 'math 'nan)))
(define math.pi (lambda () (ffi 'math 'pi)))

(define math.acos (lambda (x) (ffi 'math 'acos x)))
(define math.asin (lambda (x) (ffi 'math 'asin x)))
(define math.atan (lambda (x) (ffi 'math 'atan x)))
(define math.atan2 (lambda (y x) (ffi 'math 'atan2 y x)))
(define math.ceil (lambda (x) (ffi 'math 'ceil x)))
(define math.copysign (lambda (x y) (ffi 'math 'copysign x y)))
(define math.cos (lambda (x) (ffi 'math 'cos x)))
(define math.degrees (lambda (x) (ffi 'math 'degrees x)))
(define math.exp (lambda (x) (ffi 'math 'exp x)))
(define math.factorial (lambda (x) (ffi 'math 'factorial x)))
(define math.floor (lambda (x) (ffi 'math 'floor x)))
(define math.gcd (lambda (x y) (ffi 'math 'gcd x y)))
(define math.hypot (lambda (x y) (ffi 'math 'hypot x y)))
(define math.log (lambda (x & args) (
    eval (join (quote (ffi 'math 'log x)) args)
)))
(define math.radians (lambda (x) (ffi 'math 'radians x)))
(define math.sin (lambda (x) (ffi 'math 'sin x)))
(define math.sqrt (lambda (x) (ffi 'math 'sqrt x)))
(define math.tan (lambda (x) (ffi 'math 'tan x)))
(define math.trunc (lambda (x) (ffi 'math 'trunc x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random

(define random.choice (lambda (choices) (ffi 'random 'choice choices)))
(define random.choices (lambda (pop n wts cum) (
    ffi 'random.choices pop n wts cum
)))
(define random.gauss (lambda (mu sigma) (ffi 'random 'gauss mu sigma)))
(define random.getrandbits (lambda (n) (ffi 'random 'getrandbits n)))
(define random.random (lambda () (ffi 'random 'random)))
(define random.randrange (lambda (& args) (
    eval (join (quote (ffi 'random 'randrange)) args)
)))
(define random.sample (lambda (pop n) (ffi 'random 'sample pop n)))
(define random.shuffle (lambda (lst) (ffi 'random.shuffle lst)))
(define random.uniform (lambda (a b) (ffi 'random 'uniform a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings

(define string.endswith (lambda (s suffix & args) (
    eval (join (quote (ffi 'string s 'endswith suffix)) args)
)))
(define string.equal? (lambda (s t) (ffi 'string.equal? s t)))
(define string.find (lambda (s sub & args) (
    eval (join (quote (ffi 'string s 'find sub)) args)
)))
(define string.join (lambda (lst joiner) (
    ffi 'string joiner 'join lst
)))
(define string.length (lambda (s) (ffi 'string.length s)))
(define string.lower (lambda (s) (ffi 'string s 'lower )))
(define string.lstrip (lambda (s & args) (
    eval (join (quote (ffi 'string s 'lstrip)) args)
)))
(define string.lt? (lambda (s t) (ffi 'string.lt? s t)))
(define string.mul (lambda (s n) (ffi 'string.mul s n)))
(define string.replace (lambda (s old new & args) (
    eval (join (quote (ffi 'string s 'replace old new)) args)
)))
(define string.rfind (lambda (s sub & args) (
    eval (join (quote (ffi 'string s 'rfind sub)) args)
)))
(define string.rsplit (lambda (s & args) (
    eval (join (quote (ffi 'string s 'rsplit)) args)
)))
(define string.rstrip (lambda (s & args) (
    eval (join (quote (ffi 'string s 'rstrip)) args)
)))
(define string.split (lambda (s & args) (
    eval (join (quote (ffi 'string s 'split)) args)
)))
(define string.startswith (lambda (s suffix & args) (
    eval (join (quote (ffi 'string s 'startsswith suffix)) args)
)))
(define string.strip (lambda (s & args) (
    eval (join (quote (ffi 'string s 'strip)) args)
)))
(define string.substr (lambda (s bgn end) (ffi 'string.substr s bgn end)))
(define string.upper (lambda (s) (ffi 'string s 'upper )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; time

(define time.asctime (lambda (tt) (ffi 'time 'asctime tt)))
(define time.gmtime (lambda (t) (ffi 'time 'gmtime t)))
(define time.localtime (lambda (t) (ffi 'time 'localtime t)))
(define time.mktime (lambda (tt) (ffi 'time 'mktime tt)))
(define time.sleep (lambda (t) (ffi 'time 'sleep t)))
(define time.strftime (lambda (fmt tt) (ffi 'time 'strftime fmt tt)))
(define time.strptime (lambda (str fmt) (ffi 'time 'strptime str fmt)))
(define time.time (lambda () (ffi 'time 'time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timing functions by rep count or total time

(define timeit-reps (lambda (f reps) ( do
    (define k reps)
    (define t0 (time.time))
    (define g (lambda () ( do
        (cond
            ((lt? reps 1) ())
            (#t (do
                (f)
                (set! reps (sub reps 1))
                #t
            ))
        )
    )))
    (while g)
    (div (sub (time.time) t0) k)
)))

(define timeit-time (lambda (f dt) ( do
    (define reps 0)
    (define t0 (time.time))
    (define g (lambda () ( do
        (cond
            ((lt? dt (sub (time.time) t0)) ())
            (#t (do
                (f)
                (set! reps (add reps 1))
                #t
            ))
        )
    )))
    (while g)
    (div (sub (time.time) t0) reps)
)))

;; EOF
