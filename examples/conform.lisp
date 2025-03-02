;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/conform.scm

(define else #t)

(def (sort-list obj pred)

  (def (loop l)
    (if (and (pair? l) (pair? (cdr l)))
        (split-list l () ())
        l))

  (def (split-list l one two)
    (if (pair? l)
        (split-list (cdr l) two (cons (car l) one))
        (merge (loop one) (loop two))))

  (def (merge one two)
    (cond ((null? one) two)
          ((pred (car two) (car one))
           (cons (car two)
                 (merge (cdr two) one)))
          (else
           (cons (car one)
                 (merge (cdr one) two)))))

  (loop obj))


(define items (range 1000 -1 -1))
(timeit (lambda (_) (sort-list items lt?)) 1)


;;;


(def (memq element lst)
    (if
        (equal? element (car lst))
        #t
        (memq element (cdr lst))
    )
)

(def (adjoin element set)
  (if (memq element set) set (cons element set)))

(def (eliminate element set)
  (cond ((null? set) set)
        ((eq? element (car set)) (cdr set))
        (else (cons (car set) (eliminate element (cdr set))))))

(def (intersect list1 list2)
  (def (loop l)
    (cond ((null? l) ())
          ((memq (car l) list2) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))
  )
  (loop list1)
)

(def (union list1 list2)
  (if (null? list1)
      list2
      (union (cdr list1)
             (adjoin (car list1) list2))))


(define items (range 1 300 1))
(timeit (lambda (_) (union items items)) 1)

;;;


