;; lwp.lisp - runtime, lots of which is from other sources
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

;; to accompany quasiquote
(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

;; used everywhere
(define null? (lambda (x) (if (eq? x ()) #t ())))
(define pair? (lambda (x) (if (eq? (type x) 'pair) #t ())))

;; ditto
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define caddddr (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))

;; define do
(special begin$2 (lambda (__special_begin$2_a__ __special_begin$2_b__)
     (if  ;; can't remember where i saw this but i like it
        (eval __special_begin$2_a__ 1)
        (eval __special_begin$2_b__ 1)
        (eval __special_begin$2_b__ 1)
    )
))

(special do (lambda (& __special_do_args__)
    (eval (do$ __special_do_args__) 1)))

(define do$ (lambda (__special_do$_args__)
    (if
        (null? __special_do$_args__)
        ()
        (if
            (null? (cdr __special_do$_args__))
            (car __special_do$_args__)
            `(begin$2
                ,(car __special_do$_args__)
                ,(do$ (cdr __special_do$_args__)))
        )
    )
))

;; define cond

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

;; here we go

;; define negation and addition first
(define neg     (lambda (x) (sub 0 x)))
(define add$2   (lambda (x y) (sub x (neg y))))

(special add (lambda (__special_add_x__ & __special_add_args__)
    (eval (add$ __special_add_x__ __special_add_args__) 1)))

(define add$ (lambda (x args)
    (if
        (null? args)
        `,x
        `(add$2 ,x ,(add$ (car args) (cdr args)))
    )
))

;; oh, and mod
(define mod     (lambda (n d) (sub n (mul d (div n d)))))

;; absolute value
(define abs (lambda (x)
    (if
        (lt? x 0)
        (neg x)
        x
    )
))

;; copysign
(define copysign (lambda (x y)
    (if
        (lt? y 0)
        (neg (abs x))
        (abs x)
)))

;; some comparison predicates
(define le? (lambda (x y) (or (lt? x y) (equal? x y))))
(define ge? (lambda (x y) (not (lt? x y))))
(define gt? (lambda (x y) (not (le? x y))))

;; bitwise ops from nand
(define bnot    (lambda (x) (nand x x)))
(define band    (lambda (x y) (bnot (nand x y))))
(define bor     (lambda (x y) (nand (bnot x) (bnot y))))
(define bxor    (lambda (x y) (band (nand x y) (bor x y))))

;; (signed) shifts
(define lshift  (lambda (x n) (
    cond
        ((equal? n 0)   x)
        ((equal? n 1)   (add x x))
        (#t             (lshift (lshift x (sub n 1)) 1))
)))

(define rshift  (lambda (x n) (
    cond
        ((equal? n 0)   x)
        ((equal? n 1)   (div x 2))
        (#t             (rshift (rshift x (sub n 1)) 1))
)))

(define list    (lambda (& args) args))

(define bool (lambda (x) (if x #t ())))

;; and or not

(special and (lambda (& __special_and_args__) (
    cond
        ((null? __special_and_args__) ())
        ((null? (cdr __special_and_args__))
            (eval (car __special_and_args__)))
        ((eval (car __special_and_args__)) 
             (eval (join (quote (and)) (cdr __special_and_args__))))
        (#t ())
)))

(special or (lambda (& __special_or_args__) (
    cond
        ((null? __special_or_args__) ())
        ((eval (car __special_or_args__))  #t)
        (#t (eval (join (quote (or)) (cdr __special_or_args__))))
)))

(define not (lambda (x) (if x () #t)))

;;

(define join (lambda (x y)
    (if
        (null? x)
        y
        (cons (car x) (join (cdr x) y))
    )
))

(define length (lambda (l) (do
        (define helper (lambda (l n)
            (if
                (null? l)
                n
                (helper (cdr l) (add n 1))
            )
        ))

        (helper l 0)
)))

(special assert (lambda (__special_assert_sexpr__)
    (if
        (eval __special_assert_sexpr__)
        ()
        (error (>string __special_assert_sexpr__))
    )
))

;; signed integer multiplication from subtraction and right shift (division)
(define smul (lambda (x y) (do
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
)))


;; sicp p.158-165
(define accumulate (lambda (f initial sequence) (
    if
        (null? sequence)
        initial
        (f (car sequence) (accumulate f initial (cdr sequence)))
)))

(define fold-left (lambda (op initial sequence) ( do
    (define iter (lambda (result rest) (
        if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))
    )))
    (iter initial sequence)
)))

(define map1 (lambda (f lst)
    (if
        (null? lst)
        ()
        (cons (f (car lst)) (map1 f (cdr lst)))
    )
))

(define accumulate-n (lambda (f initial sequences) (
    if
        (null? (car sequences)) ;; assume other seqs are empty too
        ()
        (cons
            (accumulate f initial (map1 car sequences))
            (accumulate-n f initial (map1 cdr sequences))
        )
)))

(define transpose (lambda (lists) (
    accumulate-n cons () lists
)))

(define map (lambda (f & lists) (do
    (define g (lambda (tuple) (eval (join (quote (f)) tuple))))
    (map1 g (transpose lists))
)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time. it's an ad-hoc queue

(define list-builder (lambda () ( do
    (define ht (list () ()))

    (define get (lambda () (car ht)))

    (define add (lambda (x) ( do
        (define node (cons x ()))
        (if
            (null? (car ht))
            ( do
                (set-car! ht node)
                (set-cdr! ht node)
            )
            (do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            )
        )
        dispatch
    )))

    (define dispatch (lambda (op & args)
        (cond
            ((eq? op (quote add))
                (if
                    (equal? (length args) 1)
                    (add (car args))
                    (error "add takes a single arg")
                )
            )
            ((eq? op (quote extend))
                (if
                    (equal? (length args) 1)
                    ( do
                        (foreach add (car args))
                        dispatch
                    )
                    (error "extend takes a single list arg")
                )
            )
            ((eq? op (quote get)) (car ht))
        )
    ))

    dispatch
)))

;; save some (eval (join (quote (sym)) args)) awkwardness
(special apply* (lambda (sym & args) ( do
    (define lb (list-builder))
    (lb (quote add) sym)
    (define f (lambda (lst) ( do
        (define x (eval lst))
        (if
            (pair? x)
            (lb (quote extend) x)
            (lb (quote add) x)
        )
    )))
    (foreach f args)  ;; lack of tco is killing me :-\
    (eval (lb (quote get)) 1)
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; it's still python-thinking today :D

(define iter (lambda (lst fin) (do
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
)))

(define enumerate (lambda (lst fin) (do
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
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue

(define queue (lambda () ( do
    (define h ())
    (define t ())

    (define dispatch (lambda (op & args)
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
    ))
    dispatch
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let*

(special let* (lambda (__special_lets_vdefs__ __special_lets_body__)
    (eval (let*$ __special_lets_vdefs__ __special_lets_body__) 1)))

(define let*$ (lambda (vdefs body) ( do
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
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let

(special let (lambda (__special_let_vdefs__ __special_let_body__)
    (eval (let$ __special_let_vdefs__ __special_let_body__) 1)))

(define let$ (lambda (vdefs body) ( do
    (define vdecls (transpose vdefs))
    (define vars (car vdecls))
    (define vals (cadr vdecls))
    `((lambda (,@vars) ,body) ,@vals)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; last

(define last (lambda (l)
    ((lambda (c)
        (if
            (null? (cdr l))
            (car l)
            (if (set! l (cdr l)) () (c c)) ;; use if to sequence
        )
    ) (call/cc (lambda (cc) cc)) )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def

(special def (lambda (__special_def_funcargs__ & __special_def_body__)
    (eval (def$ __special_def_funcargs__ __special_def_body__) 1)))

(define def$ (lambda (funcargs body) ( do
    (if (or (not (pair? funcargs)) (null? funcargs))
        (error "def needs a func to define!")
        ())
    (define f (car funcargs))
    (define a (cdr funcargs))
    `(define ,f (lambda (,@a) (do ,@body)))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associative table

(define table (lambda (compare) ( do
    (define items ())
    (define dispatch (lambda (m & args) ( do
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
    )))
    dispatch
)))

(define table$find (lambda (items key compare)
    (cond
      ((null? items) ())
      ((compare (car (car items)) key) (car items))
      (#t (table$find (cdr items) key compare))
    )
))

(define table$delete (lambda (items key compare) ( do
    (define prev ())
    (define helper (lambda (assoc key) ( do
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
    )))
    (helper items key)
)))


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
    (if
        flag
        (c (f))
        ()
    )
)))

;; loop until f returns true
(define until (lambda (f) ( do
    (define c ())
    (define flag (call/cc (lambda (cc) (do (set! c cc) ()))))
    (if
        flag
        ()
        (c (f))
    )
)))

;; call f a given number of times as (f counter)
(def (for f n)
    (define i 0)
    (define c (call/cc (lambda (cc) cc)))
    (if
        (ge? i n)
        ()
        ( do
            (f i)
            (set! i (add i 1))
            (c c)
        )
    )
)

(define reverse (lambda (lst) ( do
    (define r ())
    (define f (lambda ()
        (if
            (null? lst)
            ()
            (do
                (set! r (cons (car lst) r))
                (set! lst (cdr lst))
                #t
            )
        )
    ))
    (while f)
    r
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmarking

(def (timeit f n)
    (define t0 (time 'time))
    (for f n)
    (define t1 (time 'time))
    (define dt (sub t1 t0))
    (if (lt? dt 1e-7) (set! dt 1e-7) ())
    (if (lt? n 1) (set! n 1) ())
    (list n dt (mul 1e6 (div dt n)) (div n dt))
)


;; EOF
