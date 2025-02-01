;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rec2.lisp - runtime, lots of which is from other sources
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
(define null? (lambda (x) (if (eq? x ()) #t ())))
(define pair? (lambda (x) (if (eq? (type x) 'pair) #t ())))
(define list  (lambda (& args) args))

;; ditto
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))

;; }}}
;; {{{ last

(define do$2 (lambda (a b) (if a b b)))

(define last (lambda (l)
    (if
        (null? l)
        ()
        (do$2
            (define res ())
            (do$2
                (define f (lambda ()
                    (if
                        (null? (cdr l))
                        (do$2
                            (set! res (car l))
                            ()
                        )
                        (do$2
                            (set! l (cdr l))
                            #t
                        )
                    )
                ))
                (do$2
                    (while f)
                    res
                )
            )
        )
    )
))

;; }}}
;; {{{ foreach
;; call f for each element of lst

(define foreach (lambda (f lst) (do
    (define g (lambda ()
        (if
            (null? lst)
            ()
            ( do
                (f (car lst))
                (set! lst (cdr lst))
                #t
            )
        )
    ))
    (while g)
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
;; {{{ cond

(special cond (lambda (& __special_cond_pcs__)
    (eval (cond$ __special_cond_pcs__) 1)))

(define cond$ (lambda (__special_cond_pcs__)
    (if
        (null? __special_cond_pcs__)
        ()
        `(if
            ,(car  (car __special_cond_pcs__))
            ,(cadr (car __special_cond_pcs__))
            ,(cond$ (cdr __special_cond_pcs__)))
    )
))

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
(def (add$2 x y) (sub x (neg y)))

(special add (lambda (__special_add_x__ & __special_add_args__)
    (eval (add$ __special_add_x__ __special_add_args__) 1)))

(def (add$ x args)
    (if
        (null? args)
        `,x
        `(add$2 ,x ,(add$ (car args) (cdr args)))
    )
)

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

;; signed integer multiplication from subtraction and right shift (division)
(def (smul x y)
    (define umul (lambda (x y z) (
        cond
            ((equal? y 1) x) ;; y could have been -1 on entry to smul
            ((equal? 0 x) z)
            ((equal? 0 (band x 0x1)) (umul (rshift x 1) (add y y) z))
            (#t (umul (rshift x 1) (add y y) (add z y)))
    )))
    (cond
        ((equal? x 0) 0)
        ((equal? y 0) 0)
        ((lt? x 0) (neg (smul (neg x) y)))
        ((equal? x 1) y)
        ((equal? y 1) x)
        (#t (copysign (umul x (abs y) 0) y))
    )
)

;(smul 37 23)  ;; 851

;; }}}
;; {{{ comparison predicates

(def (le? x y) (if (lt? x y) #t (if (equal? x y) #t ())))
(def (ge? x y) (not (lt? x y)))
(def (gt? x y) (lt? y x))

;; }}}
;; {{{ and or not

(special and (lambda (& __special_and_args__)
    (if
        (null? __special_and_args__)
        ()
        (eval (and$ __special_and_args__) 1)
    )
))

(def (and$ args)
    (if
        (null? args)
        #t
        `(if
            (eval ,(car args))
            ,(and$ (cdr args))
            ()
        )
    )
)

(special or (lambda (& __special_or_args__)
    (if
        (null? __special_or_args__)
        ()
        (eval (or$ __special_or_args__) 1)
    )
))

(def (or$ args)
    (if
        (null? args)
        ()
        `(if
            (eval ,(car args))
            #t
            ,(or$ (cdr args))
        )
    )
)

(def (not x) (if (null? x) #t ()))

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
;; {{{ join

(def (join x y)
    (define lb (list-builder))
    (lb 'extend x)
    (lb 'extend y)
    (lb 'get)
)

;; }}}
;; {{{ reverse

(def (reverse l)
    (define res ())
    (def (f)
        (if
            (null? l)
            ()
            (do 
                (set! res (cons (car l) res))
                (set! l (cdr l))
                #t
            )
        )
    )
    (while f)
    res
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
    (def (f)
        (if
            (null? l)
            ()
            (do
                (set! n (add n 1))
                (set! l (cdr l))
                #t
            )
        )
    )
    (while f)
    n
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
    (if
        (null? (car sequences)) ;; assume other seqs are empty too
        ()
        (cons
            (accumulate   f initial (map1 car sequences))
            (accumulate-n f initial (map1 cdr sequences))
        )
    )
)

(def (transpose lists) (accumulate-n cons () lists))

(def (map f & lists)
    (def (g tuple) (apply f tuple))
    (map1 g (transpose lists))
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
;; {{{ queue

(def (queue)
    (define h ())
    (define t ())

    (def (dispatch op & args)
        (cond
            ((eq? op 'enqueue)
                (if
                    (equal? (length args) 1)
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
            ((eq? op 'dequeue)
                (if
                    (null? args)
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
            ((eq? op 'empty?) (eq? h ()))
            ((eq? op 'enqueue-many)
                (if
                    (and (equal? (length args) 1) (pair? (car args)))
                    ( do
                        (foreach enqueue (car args))
                        dispatch
                    )
                    (error "enqueue-many takes one list arg")
                )
            )
            ((eq? op 'get-all) h)
        )
    )
    dispatch
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
;; {{{ looping: loop, for, iter-func

;; call f in a loop forever
(def (loop f)
    (while (lambda () (do (f) #t)))
)

;; call f a given number of times as (f counter)
(def (for f start stop step)
    (if (lt? step 1) (error "step must be positive") ())
    (define i start)
    (def (g)
        (if
            (lt? i stop)
            ( do
                (f i)
                (set! i (add i step))
                #t
            )
            ()
        )
    )
    (while g)
)

(def (iter-func f x0 n)
    (def (g)
        (if
            (lt? n 1)
            ()
            (do
                (set! n (sub n 1)) 
                (set! x0 (f x0))
                #t
            )
        )
    )
    (while g)
    x0
)

;; }}}
;; {{{ gcd

(def (gcd x y)
    (define g 1)
    (def (run)
        (if
            (equal? y 0)
            (do
                (set! g x)
                ()
            )
            (do
                (define r (mod x y))
                (set! x y)
                (set! y r)
                #t
            )
        )
    )
    (cond
        ((lt? x y) (gcd y x))
        ((equal? y 0) x)
        ((equal? x 0) 1)
        (#t (do
          (while run)
          g
        ))
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
;; {{{ factorial benchmark

(def (!bench)
    (def (!1  n)
        (def (f x)
            (cons
                (mul (car x) (cdr x))
                (sub (cdr x) 1)
            )
        )
        (car (iter-func f (cons 1 n) n))
    )

    (def (!2 n)
        (define n! 1)
        (def (f)
            (if
                (lt? n 2)
                ()
                (do
                    (set! n! (mul n! n))
                    (set! n  (sub n 1))
                    #t
                )
            )
        )
        (while f)
        n!
    )

    (def (!3 n)
        (if
            (lt? n 2)
            1
            (mul n (!3 (sub n 1)))
        )
    )

    (def (!4 n)
         (define n! 1)
         (def (f k) (set! n! (mul n! k)))
         (for f 1 n 1)
         n!
    )

    (def (!5 n)
        (def (iter n! k)
            (if
                (lt? k 2)
                n!
                (iter (mul n! k) (sub k 1))
            )
        )
        (iter 1 n)
    )

    (def (!6 n)
         (math 'factorial n)
    )

    (def (!7 n)
        (fold-left mul 2 (range 3 (add n 1) 1))
    )

    (def (!8 n)
        (def (f x)
            (set! n (sub n 1))
            (mul n x)
        )
        (iter-func f n (sub n 1))
    )

    (define n 100)
    (define reps 50)

    (print "nil" (timeit (lambda (_) ()) reps))
    (print "!1 " (timeit (lambda (_) (!1 n)) reps))
    (print "!2 " (timeit (lambda (_) (!2 n)) reps))
    (print "!3 " (timeit (lambda (_) (!3 60)) reps))
    (print "!4 " (timeit (lambda (_) (!4 n)) reps))
    (print "!5 " (timeit (lambda (_) (!5 60)) reps))
    (print "!6 " (timeit (lambda (_) (!6 n)) reps))
    (print "!7 " (timeit (lambda (_) (!7 n)) reps))
    (print "!8 " (timeit (lambda (_) (!8 n)) reps))
)

(def (bench) (cadr (timeit (lambda (_) (!bench)) 1)))

;;   home slow  4.24    723fc03131132b523971114b112e95143e989664
;;              3.15    6d51d855dbf9de31586dbe520b98a41a84ac7091
;;              3.18    77f398553d458833f7a7f14061ce9f57b658d3bf

(bench)

;; }}}

;; EOF
