(define (fac n)
  (cond ((< n 2) 1)
    (#t (* n (fac (- n 1))))))

(display (fac 5))
(newline)
