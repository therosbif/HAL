(define (null? l) (eq? l '()))

(define (merge-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2)))
        (#t                    (cons (car l2) (merge-lists l1 (cdr l2))))))

(define (split-half l l1 l2)
  (cond ((null? l) (cons l1 l2))
        ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2))
        (#t (split-half (cdr (cdr l))
                        (cons (car l) l1)
                        (cons (car (cdr l)) l2)))))

(define (merge-sort lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (#t (let ((lsts (split-half lst '() '())))
              (merge-lists (merge-sort (car lsts))
                           (merge-sort (cdr lsts)))))))

(merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))
