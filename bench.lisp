;; bench.lisp - stdlib-based code to benchmark lisps

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
