#lang racket

(provide iter)

(define (iter n f)
  (if (equal? n 0)
      ; end case : f^0(x) = x
      (lambda (x) x)
      ; recursive case : f^n(x) = f(f^(n-1)(x))
      ; let으로 f^(n-1)(x)를 정의함
      (let ((f2 (iter (- n 1) f)))
        (lambda (x) (f (f2 x)))
        )
      )
  )