(define (not x)
  (if x
      #f
      #t))

(define (null? obj)
  (eq? obj '()))

(define (lst . objs) objs)

(define (id obj) obj)

(define (flip func)
  (lambda (a b)
    (func b a)))

(define (curry func arg1)
  (lambda (arg)
    (apply func (cons arg1 arg))))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero?
  (curry eq? 0))

(define positive?
  (curry < 0))

(define negative?
  (curry > 0))

(define (odd? x)
  (eq? (mod x 2) 1))

(define (even? x)
  (not (odd? x)))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)

(define reduce fold)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 1 lst))

(define (and . lst)
  (fold && #t lst))

(define (or . lst)
  (fold || #f lst))

(define (max first . lst)
  (fold (lambda (old new)
                (if (< old new) new old))
        first
        lst))

(define (min first . lst)
  (fold (lambda (old new)
                (if (< old new) old new))
        first
        lst))

(define (length lst)
  (fold (lambda (len elem)
                (+ len 1))
        0
        lst))

(define (reverse lst)
  (fold (flip cons) '() lst))


(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc) (pred (op next)))
        next
        acc)))

(define (member obj lst)
  (fold (mem-helper (curry eq? obj) id) #f lst))

(define (assoc obj alist)
  (fold (mem-helper (curry eq? obj) car) #f alist))

(define (map func lst)
  (foldr  (lambda (elem accum) (cons (func elem) accum))
          '()
          lst))

(define (filter pred lst)
  (foldr (lambda (elem accum)
                  (if (pred elem)
                      (cons elem accum)
                      accum))
          '()
          lst))
