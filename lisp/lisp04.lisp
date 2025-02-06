;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp04.lisp - runtime, lots of which is from other sources
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

;; {{{ basics

;; to accompany quasiquote
(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

;; used everywhere
(define pair? (lambda (x) (if (eq? (type x) 'pair) #t ())))
(define list  (lambda (& args) args))

;; ditto
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))

;; }}}
;; {{{ begin

(define begin do)

;; }}}
;; {{{ foreach
;; call f for each element of lst

(define foreach (lambda (f lst) ( do
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? lst)
        ()
        ( do
            (f (car lst))
            (set! lst (cdr lst))
            (c c)
        )
    )
)))

;; }}}
;; {{{ list-builder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time. it's an ad-hoc queue

(define list-builder (lambda () ( do
    (define ht (list () ()))
    (define add (lambda (x) ( do
        (define node (cons x ()))
        (if
            (null? (car ht))
            ( do
                (set-car! ht node)
                (set-cdr! ht node)
            )
            ( do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            )
        )
        dispatch
    )))
    (define dispatch (lambda (op & args)
        (if
            (eq? op 'add)
            (if
                (null? (cdr args))
                (add (car args))
                (error "add takes a single arg")
            )
            (if
                (eq? op 'extend)
                (if
                    (null? (cdr args))
                    ( do
                        (foreach add (car args))
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
)))

;; }}}
;; {{{ def

(special def (lambda (__special_def_funcargs__ & __special_def_body__)
    (eval (def$ __special_def_funcargs__ __special_def_body__) 1)))

(define def$ (lambda (funcargs body) ( do
    (if
        (pair? funcargs)
        ()
        (error "def needs a func to define!")
    )
    (define f (car funcargs))
    (define a (cdr funcargs))
    `(define ,f (lambda (,@a) (do ,@body)))
)))

;; }}}
;; {{{ bitwise ops

;; bitwise ops from nand
(def (bnot x)   (nand x x))
(def (band x y) (bnot (nand x y)))
(def (bor  x y) (nand (bnot x) (bnot y)))
(def (bxor x y) (band (nand x y) (bor x y)))

;; }}}
;; {{{ arithmetic

(def (neg x) (sub 0 x))
(def (add x y) (sub x (neg y)))

;; oh, and mod
(def (mod n d) (sub n (mul d (div n d))))

;; absolute value
(def (abs x)
    (if
        (lt? x 0)
        (neg x)
        x
    )
)

;; copysign
(def (copysign x y)
    (if
        (lt? y 0)
        (neg (abs x))
        (abs x)
    )
)

;; (signed) shifts
(def (lshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (add x x))
        (#t             (lshift (lshift x (sub n 1)) 1))
    )
)

(def (rshift x n)
    (cond
        ((equal? n 0)   x)
        ((equal? n 1)   (div x 2))
        (#t             (rshift (rshift x (sub n 1)) 1))
    )
)

;; }}}
;; {{{ comparison predicates

(def (le? x y) (if (lt? x y) #t (if (equal? x y) #t ())))
(def (ge? x y) (not (lt? x y)))
(def (gt? x y) (lt? y x))

;; }}}
;; {{{ and or not

(special and (lambda (& __special_and_args__)
    ((lambda (c)
        (cond
            ((null? __special_and_args__) ())
            ((null? (cdr __special_and_args__))
                (eval (car __special_and_args__)))
            ((eval (car __special_and_args__)) ( do
                (set! __special_and_args__ (cdr __special_and_args__))
                (c c)
            ))
            (#t ())
        )
    ) (call/cc (lambda (cc) cc)) )
))

(special or (lambda (& __special_or_args__)
    ((lambda (c)
        (cond
            ((null? __special_or_args__) ())
            ((eval (car __special_or_args__)) #t)
            (#t ( do
                (set! __special_or_args__ (cdr __special_or_args__))
                (c c)
            ))
        )
    ) (call/cc (lambda (cc) cc)) )
))

(def (not x) (if (eq? x ()) #t ()))

;; }}}
;; {{{ assert

(special assert (lambda (__special_assert_sexpr__)
    (if
        (eval __special_assert_sexpr__)
        ()
        (error (>string __special_assert_sexpr__))
    )
))

;; }}}
;; {{{ reverse

(def (reverse l)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        r
        ( do
            (set! r (cons (car l) r))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ apply

(def (apply sym args) (eval (cons sym args)))

;; }}}
;; {{{ iter and enumerate

(def (iter lst fin)
    (define item ())
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (car lst))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

(def (enumerate lst fin)
    (define index 0)
    (define item fin)
    (define next (lambda ()
        (if
            (null? lst)
            fin
            (do
                    (set! item (list index (car lst)))
                    (set! index (add index 1))
                    (set! lst (cdr lst))
                    item
            )
        )
    ))
    next
)

;; }}}
;; {{{ length

(def (length l)
    (define n 0)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? l)
        n
        ( do
            (set! n (add n 1))
            (set! l (cdr l))
            (c c)
        )
    )
)

;; }}}
;; {{{ fold, transpose, map
;; sicp p.158-165 with interface tweaks
(def (fold-left f initial sequence)
    (define r initial)
    (foreach (lambda (elt) (set! r (f elt r))) sequence)
    r
)

(define reduce fold-left)  ;; python nomenclature

(def (fold-right f initial sequence)
      (fold-left f initial (reverse sequence)))

(define accumulate fold-right)  ;; sicp nomenclature

;(fold-left  cons () (list 1 4 9))  ;; (9 4 1)    (cons 9 (cons 4 (cons 1 ())))
;(fold-right cons () (list 1 4 9))  ;; (1 4 9)    (cons 1 (cons 4 (cons 9 ())))

(def (map1 f lst)
    (def (g elt r) (cons (f elt) r))
    (fold-right g () lst)
)

(def (accumulate-n f initial sequences)
    (define r ())
    (define c (call/cc (lambda (cc) cc)))
    (if
        (null? (car sequences))
        (reverse r)
        ( do
            (set! r (cons (accumulate f initial (map1 car sequences)) r))
            (set! sequences (map1 cdr sequences))
            (c c)
        )
    )
)

(def (transpose lists) (accumulate-n cons () lists))

(def (map f & lists)
    (def (g tuple) (apply f tuple))
    (map1 g (transpose lists))
)

;; }}}
;; {{{ join

(def (join x y)
    (cond
        ((null? x) y)
        ((null? y) x)
        ((null? (cdr x)) (cons (car x) y))
        (#t (fold-right cons (fold-right cons () y) x))
    )
)

;; }}}
;; {{{ queue

(def (queue)
    (define h ())
    (define t ())

    (def (dispatch op & args)
        (cond
            ((eq? op (quote enqueue))
                (if
                    (equal? (length args ) 1)
                    ( do
                        (define node (cons (car args) ()))
                        (if
                            (null? h)
                            (set! h node)
                            (set-cdr! t node)
                        )
                        (set! t node)
                        ()
                    )
                    (error "enqueue takes one arg")
                )
            )
            ((eq? op (quote dequeue))
                (if
                    (equal? (length args) 0)
                        (if
                            (null? h)
                            (error "queue is empty")
                            ( let (
                                (ret (car h)))
                                (do
                                    (set! h (cdr h))
                                    (if (null? h) (set! t ()) ())
                                    ret
                                )
                            )
                        )
                    (error "dequeue takes no args")
                )
            )
            ((eq? op (quote empty?)) (eq? h ()))
            ((eq? op (quote enqueue-many))
                (if
                    (and (equal? (length args) 1) (pair? (car args)))
                    ( do
                        (foreach enqueue (car args))
                        dispatch
                    )
                    (error "enqueue-many takes one list arg")
                )
            )
            ((eq? op (quote get-all)) h)
        )
    )
    dispatch
)


;; }}}
;; {{{ let

(special let (lambda (__special_let_vdefs__ __special_let_body__)
    (eval (let$ __special_let_vdefs__ __special_let_body__) 1)))

(def (let$ vdefs body)
    (define vdecls (transpose vdefs))
    (define vars (car vdecls))
    (define vals (cadr vdecls))
    `((lambda (,@vars) ,body) ,@vals)
)

;; }}}
;; {{{ let*

(special let* (lambda (__special_lets_vdefs__ __special_lets_body__)
    (eval (let*$ __special_lets_vdefs__ __special_lets_body__) 1)))

(def (let*$ vdefs body)
    (if
        (null? vdefs)
        body
        ( do
            (define kv (car vdefs))
            (set! vdefs (cdr vdefs))
            (define k (car kv))
            (define v (cadr kv))
          `((lambda (,k) ,(let*$ vdefs body)) ,v)
        )
    )
)

;; }}}
;; {{{ letrec
;; i saw this (define x ()) ... (set! x value) on stackoverflow somewhere

(special letrec (lambda (__special_letrec_decls__ __special_letrec_body__)
    (eval (letrec$ __special_letrec_decls__ __special_letrec_body__) 1)))

(def (letrec$ decls & body)
    (define names (map1 car decls))
    (define values (map1 cadr decls))
    (def (declare var) `(define ,var ()))
    (def (initialize var-value) `(set! ,(car var-value) ,(cadr var-value)))
    (def (declare-all) (map1 declare names))
    (def (initialize-all) (map1 initialize decls))
    `((lambda () ( do ,@(declare-all) ,@(initialize-all) ,@body)))
)

;; }}}
;; {{{ associative table

(def (table compare)
    (define items ())
    (def (dispatch m & args)
        (cond
            ((eq? m 'known) (not (null? (table$find items key compare))))
            ((eq? m 'del) (set! items (table$delete items (car args) compare)))
            ((eq? m 'get) ( do
                (let* (
                    (key (car args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ()
                        (cadr node)
                    )
                )
            ))
            ((eq? m 'iter) ( do
                (let ((lst items))
                    (lambda ()
                        (if
                            (null? lst)
                            ()
                            ( do
                                (define ret (car lst))
                                (set! lst (cdr lst))
                                ret
                            )
                        )
                    )
                )
            ))
            ((eq? m 'len) (length items))
            ((eq? m 'raw) items)
            ((eq? m 'set) ( do
                (let* (
                    (key (car args))
                    (value (cadr args))
                    (node (table$find items key compare)))
                    (if
                        (null? node)
                        ( do
                            (let* (
                                (node (cons key (cons value ()))))
                                (set! items (cons node items)))
                        )
                        (set-car! (cdr node) value)
                    )
                )
            ))
            (#t (error "unknown method"))
        )
    )
    dispatch
)

(def (table$find items key compare)
    (cond
      ((null? items) ())
      ((compare (car (car items)) key) (car items))
      (#t (table$find (cdr items) key compare))
    )
)

(def (table$delete items key compare)
    (define prev ())
    (def (helper assoc key)
        (cond
            ((null? assoc) items)
            ((compare (car (car assoc)) key) (do
                (cond
                    ((null? prev) (cdr assoc))
                    (#t (do (set-cdr! prev (cdr assoc)) items))
                )
            ))
            (#t ( do
                (set! prev assoc)
                (helper (cdr assoc) key)
            ))
        )
    )
    (helper items key)
)

;; }}}
;; {{{ looping: loop, for

;; call f in a loop forever
(def (loop f)
    (define c (call/cc (lambda (cc) cc)))
    (f)
    (c c)
)

;; call f in a loop forever or until (break) is called
(def (loop-with-break f)
    (define brk ())
    (def (break) (brk ()))
    (def (g)
        (define c (call/cc (lambda (cc) cc)))
        (f break)
        (c c)
    )
    (set! brk (call/cc (lambda (cc) cc)))
    (if
        brk
        (g)
        ()
    )
)

;; call f a given number of times as (f counter)
(def (for f start stop step)
    (if (lt? step 1) (error "step must be positive") ())
    (define i start)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? i stop)
        ( do
            (f i)
            (set! i (add i step))
            (c c)
        )
        ()
    )
)

;; }}}
;; {{{ iterate (compose with itself) a function

(def (iter-func f x0 n)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (lt? n 1)
        x0
        (do
            (set! x0 (f x0))
            (set! n (sub n 1))
            (c c)
        )
    )
)

;; }}}
;; {{{ benchmarking

(def (timeit f n)
    (define t0 (time 'time))
    (for f 0 n 1)
    (define t1 (time 'time))
    (define dt (sub t1 t0))
    (if (lt? dt 1e-7) (set! dt 1e-7) ())
    (if (lt? n 1) (set! n 1) ())
    (list n dt (mul 1e6 (div dt n)) (div n dt))
)

;; }}}
;; {{{ gcd

(def (gcd x y)
    (cond
        ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        (#t ( do
            (define c (call/cc (lambda (cc) cc)))
            (if
                (equal? y 0)
                x
                ( do
                    (define r (mod x y))
                    (set! x y)
                    (set! y r)
                    (c c)
                )
            )
        ))
    )
)

;; }}}
;; {{{ smul

;; signed integer multiplication from subtraction and right shift (division)
(define umul (lambda (x y accum)
    ((lambda (c)
        (if
            (equal? 0 x)
            accum
            ((lambda (& _) (c c))
                (if
                    (equal? (band x 1) 1)
                    (set! accum (add accum y))
                    ()
                )
                (set! x (div x 2))
                (set! y (mul y 2))
            )
        )
    ) (call/cc (lambda (cc) cc)))
))

(define smul (lambda (x y) (do
    (define sign 1)
    (if (lt? x 0) (set! sign (neg sign)) ())
    (if (lt? y 0) (set! sign (neg sign)) ())
    (cond
        ((equal? x 0)       0)
        ((equal? y 0)       0)
        ((equal? (abs y) 1) (copysign x sign))
        ((lt? y x)          (copysign (umul (abs y) (abs x) 0) sign))
        (#t                 (copysign (umul (abs x) (abs y) 0) sign))
    )
)))

;; }}}

;; EOF
