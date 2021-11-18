(define (not x)
  (if x
      #f
      #t))

(define (null? obj)
  (eq? obj '()))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip func)
  (lambda (a b)
    (func b a)))
