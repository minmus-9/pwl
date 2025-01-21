;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp.lisp - stuff that needs lisp.py, quasiquote in particular

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
            (#t (list (set! l (cdr l)) (c c)))
        )
    ) (call/cc (lambda (cc) cc)) )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; def

(special def (lambda (funcargs body) (eval (def$ funcargs body) 1)))

(define def$ (lambda (funcargs body) ( do
    (if (or (not (list? funcargs)) (null? funcargs))
        (error "def needs a func to define!")
        ())
    (define f (car funcargs))
    (define a (cdr funcargs))
    `(define ,f (lambda (,@a) ,body))
)))


;; EOF
