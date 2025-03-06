;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fast factorial -- i ran across something like this somewhere on the
;;                   net... was an example of not having to use (define)
;;                   to create a recursive function iirc. the wikipedia
;;                   page for lambda calculus shows this technique as
;;                   well
;;
;; all of the lisps here can run this code as-is

(define ! (lambda (n)
    ((lambda (f) (f f 1 n))
        (lambda (f p k)
            (cond
                ((lt? k 2)  p)
                (#t         (f f (mul p k) (sub k 1))))))))

(define n 191)  ;; as much as the recursive lisp implementations can handle

(define f1 (lambda () (! n)))
(define f5 (lambda () (do (f1) (f1) (f1) (f1) (f1))))
(define g5 (lambda () (do (f5) (f5) (f5) (f5) (f5))))
(define h5 (lambda () (do (g5) (g5) (g5) (g5) (g5))))
(define i5 (lambda () (do (h5) (h5) (h5) (h5) (h5))))
(i5)

;; EOF
