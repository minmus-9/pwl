;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define unquote (lambda (x) (error "cannot unquote here")))
(define unquote-splicing (lambda (x) (error "cannot unquote-splicing here")))

(special begin$2 (lambda (__special_begin$2_a__ __special_begin$2_b__)
     (if
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

(define z (lambda (x)
    (cond
        ((eq? () x) (print 'nil))
        ((eq? (type x) 'list) (print 'list))
        (#t (print 'thang))
    )
))
(z ())
(z 1)
(z '(1))
