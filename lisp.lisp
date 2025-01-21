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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associative table

(define table (lambda (compare) ( do
    (define items ())
    (define dispatch (lambda (m & args) ( do
        (cond
            ((eq? m 'len) (length items))
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


;; EOF
