;; lwp.lisp - runtime, much of which is from other sources
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
(define add2    (lambda (x y) (sub x (neg y))))
(define add     (lambda (x & args)
    (if
        (null? args)
        x
        (eval (join (list (quote add) (add2 x (car args))) (cdr args)))
    )
))

;; oh, and mod
(define mod     (lambda (n d) (sub n (mul d (div n d)))))

;; absolute value
(define abs     (lambda (x) (
    cond
        ((lt? x 0)  (neg x))
        (#t         x)
)))

;; this is redefined in the math ffi interface... for pylisp.py
(define copysign (lambda (x y) (
    cond
        ((lt? y 0) (neg (abs x)))
        (#t (abs x))
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

(define bool (lambda (x) (cond (x #t) (#t ()))))

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

(define not     (lambda (x) (cond (x ()) (#t #t))))

;;

(define join (lambda (x y)
    (cond
        ((null? x)  y)
        (#t         (cons (car x) (join (cdr x) y)))
    )
))

(define last    (lambda (l) (
    cond
        ((null? l)          (error "list empty!"))
        ((null? (cdr l))    (car l))
        (#t                 (last (cdr l)))
)))

(define length (lambda (l) (do
        (define helper (lambda (l n) (
            cond
                ((null? l)  n)
                (#t         (helper (cdr l) (add n 1)))
        )))

        (helper l 0)
)))

;;; definition of let sicp p.87
(special let (lambda (__special_let_vars__ __special_let_body__) (do
    ;; (let ((x a) (y b)) body)

    (define __special_let_vdecls__ ())  ;; (x y)
    (define __special_let_vvals__ ())   ;; ((eval a) (eval b))
    ;; declare one var
    (define __special_let_decl1__ (lambda (__special_let_decl1_var__ __special_let_decl1_value__) (do
        (set! __special_let_vdecls__ (join __special_let_vdecls__ (list __special_let_decl1_var__)))
        (set! __special_let_vvals__  (join __special_let_vvals__  (list (eval __special_let_decl1_value__))))
    )))
    ;; declare the next item from vars
    (define __special_let_next__ (lambda () (do
        (__special_let_decl1__ (car (car __special_let_vars__)) (car (cdr (car __special_let_vars__))))
        (set! __special_let_vars__ (cdr __special_let_vars__))
    )))
    ;; declare everthing, then return (doit)
    (define __special_let_decls__ (lambda () (
        cond
            ((null? __special_let_vars__)   (__special_let_doit__))
            (#t (do
                    (__special_let_next__) (__special_let_decls__)
            ))
    )))
    (define __special_let_doit__ (lambda () (do
        (define __special_let_doit_head__ (join (list (quote lambda)) (list __special_let_vdecls__)))
        (define __special_let_doit_mid__  (join __special_let_doit_head__ (list __special_let_body__)))
        (define __special_let_doit_lam__  (join (list __special_let_doit_mid__) __special_let_vvals__))
        (eval __special_let_doit_lam__)
    )))
    (__special_let_decls__)
)))

(special assert (lambda (__special_assert_sexpr__) (
    cond
        ((eval __special_assert_sexpr__)   ())
        (#t (error (>string __special_assert_sexpr__)))
)))

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

(define first car)
(define cadr (lambda (l) (car (cdr l))))
(define second cadr)
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define third caddr)
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define fourth cadddr)


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

(define map1 (lambda (f lst) (
    cond
        ((null? lst) ())
        (#t (cons (f (car lst)) (map1 f (cdr lst))))
)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dingus to build a list by appending in linear time. it's an ad-hoc queue

(define list-builder (lambda () ( do
    (define ht (list () ()))

    (define get (lambda () (car ht)))

    (define add (lambda (x) ( do
        (define node (cons x ()))
        (cond
            ((null? (car ht)) ( do
                (set-car! ht node)
                (set-cdr! ht node)
            ))
            (#t (do
                (set-cdr! (cdr ht) node)
                (set-cdr! ht node)
            ))
        )
        dispatch
    )))

    (define dispatch (lambda (op & args)
        (cond
            ((eq? op (quote add))
                (cond
                    ((equal? (length args) 1) (add (car args)))
                    (#t (error "add takes a single arg"))
                )
            )
            ((eq? op (quote extend))
                (cond
                    ((equal? (length args) 1) ( do
                        (foreach add (car args))
                        dispatch
                    ))
                    (#t (error "extend takes a single list arg"))
                )
            )
            ((eq? op (quote get)) (car ht))
        )
    ))

    dispatch
)))

;; the stdlib reverse routine is quadratic time in list size; this one is
;; linear. but we can't put it into stdlib because lb fails for easy.py.
;; and i want easy.py to be able to use stdlib.

;; for rec.py and up, let's take the linear version...

(define reverse (lambda (lst) (do
    (define LB (list-builder))
    (accumulate
        (lambda (x lb) (lb (quote add) x))
        LB
        lst
    )
    (LB (quote get))
)))

;; save some (eval (join (quote (sym)) args)) awkwardness
(special eval-flattened (lambda (sym & args) ( do
    (define lb (list-builder))
    (lb (quote add) sym)
    (define f (lambda (lst) ( do
        (define x (eval lst))
        (cond
            ((pair? x) (lb (quote extend) x))
            (#t (lb (quote add) x))
        )
    )))
    (foreach f args)
    (eval (lb (quote get)))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; it's still python-thinking today :D

(define iter (lambda (lst fin) (do
    (define item ())
    (define next (lambda ()
        (cond
            ((null? lst) fin)
            (#t (do
                    (set! item (car lst))
                    (set! lst (cdr lst))
                    item
                )
            )
        )
    ))
    next
)))

(define enumerate (lambda (lst fin) (do
    (define index 0)
    (define item fin)
    (define next (lambda ()
        (cond
            ((null? lst) fin)
            (#t (do
                    (set! item (list index (car lst)))
                    (set! index (add index 1))
                    (set! lst (cdr lst))
                    item
                )
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
                (cond
                    ((equal? (length args ) 1) ( do
                        (define node (cons (car args) ()))
                        (cond
                            ((null? h) (set! h node))
                            (#t (set-cdr! t node))  ; this is why easy.py fails
                        )
                        (set! t node)
                        ()
                    ))
                    (#t (error "enqueue takes one arg"))
                )
            )
            ((eq? op (quote dequeue))
                (cond
                    ((equal? (length args) 0)
                        (cond
                            ((null? h) (error "queue is empty"))
                            (#t ( let (
                                (ret (car h)))
                                (do
                                    (set! h (cdr h))
                                    (if (null? h) (set! t ()) ())
                                    ret
                                ))
                            )
                        )
                    )
                    (#t (error "dequeue takes no args"))
                )
            )
            ((eq? op (quote empty?)) (eq? h ()))
            ((eq? op (quote enqueue-many))
                (cond
                    ((and (equal? (length args) 1) (pair? (car args))) ( do
                        (foreach enqueue (car args))
                        dispatch
                    ))
                    (#t (error "enqueue-many takes one list arg"))
                )
            )
            ((eq? op (quote get-all)) h)
        )
    ))
    dispatch
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let*

(special let* (lambda (vdefs body) (eval (let*$ vdefs body))))

(define let*$ (lambda (vdefs body) ( do
    (cond
        ((null? vdefs) body)
        (#t ( do
            (define kv (car vdefs))
            (set! vdefs (cdr vdefs))
            (define k (car kv))
            (define v (cadr kv))
          `((lambda (,k) ,(let*$ vdefs body)) ,v)))
    )
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let

(special let (lambda (vdefs body) (eval (let$ vdefs body))))

(define let$ (lambda (vdefs body) ( do
    (define vdecls (transpose vdefs))
    (define vars (car vdecls))
    (define vals (cadr vdecls))
    `((lambda (,@vars) ,body) ,@vals)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; range

(define range (lambda (n) ( do
    (define l ())
    (define c (call/cc (lambda (cc) cc)))
    (cond
        ((equal? n 0) l)
        (#t (do
            (set! n (sub n 1))
            (set! l (cons n l))
            (c c)
        ))
    )
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faster (last) esp for (do)

(define last (lambda (l)            ;; can't use (do)!
    ((lambda (c)
        (cond
            ((null? (cdr l)) (car l))
            (#t (if (set! l (cdr l)) () (c c))) ;; use if to sequence
        )
    ) (call/cc (lambda (cc) cc)) )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def

(special def (lambda (funcargs body) (eval (def$ funcargs body) 1)))

(define def$ (lambda (funcargs body) ( do
    (if (or (not (pair? funcargs)) (null? funcargs))
        (error "def needs a func to define!")
        ())
    (define f (car funcargs))
    (define a (cdr funcargs))
    `(define ,f (lambda (,@a) ,body))
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
                    (cond
                        ((null? node) ())
                        (#t (cadr node))
                    )
                )
            ))
            ((eq? m 'iter) ( do
                (let ((lst items))
                    (lambda ()
                        (cond
                            ((null? lst) ())
                            (#t ( do
                                (define ret (car lst))
                                (set! lst (cdr lst))
                                ret
                            ))
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
                    (cond
                        ((null? node) ( do
                            (let* (
                                (node (cons key (cons value ()))))
                                (set! items (cons node items)))
                        ))
                        (#t (set-car! (cdr node) value))
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

;; call f a given number of times as (f counter)
(define for (lambda (f n) ( do
    (define i 0)
    (define c (call/cc (lambda (cc) cc)))
    (cond
        ((equal? i n) ())
        (#t ( do
            (f i)
            (set! i (add i 1))
            (c c)
        ))
    )
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; XXX is this faster than the ffi one?
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
