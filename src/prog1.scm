
(define force
    (lambda (x) (x)))


(define nats
  (lambda (x)
    (cons x
      (lambda () (nats (+ x 1))))))


(define seq-head (lambda (ss)
    (car ss)))


(define seq-tail (lambda (ss)
    (force (cdr ss))))


(define seq-take (lambda (ss n)
    (cons (seq-head ss)
        (seq-take (seq-tail ss) (- n 1)))))



(force (cdr (nats 1)))


