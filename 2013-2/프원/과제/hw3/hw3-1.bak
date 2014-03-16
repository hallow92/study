 #lang racket

; DO NOT ADD OTHER COMMENTS.
; MODIFY ONLY "TODO"s in the following comments.

(define (gcd n m) ; gcd: TODO
  (if (equal? n 0) m
      (gcd (remainder m n) n)))

(define (zpn x) ; zpn: TODO
  (cond ((equal? x 'z) 0)
        ((equal? x 'p) 1)
        ((equal? x 'n) -1)))

(define (crazy2val c) ; crazy2val: TODO
  (cond ((pair? c)
         (let ((hd (car c)) ; hd: TODO
               (tl (cdr c))) ; tl: TODO
           (+ (* 2 (crazy2val tl)) (zpn hd))))
        (else (zpn c))))

(define (zpnadd a b c) ; zpnadd: TODO
  (let ((sum (+ (zpn a) (zpn b) (zpn c)))) ; sum: TODO
    (cond ((equal? sum 3) (cons 'p 'p))
          ((equal? sum 2) (cons 'z 'p))
          ((equal? sum 1) (cons 'p 'z))
          ((equal? sum 0) (cons 'z 'z))
          ((equal? sum -1) (cons 'n 'z))
          ((equal? sum -2) (cons 'z 'n))
          ((equal? sum -3) (cons 'n 'n)))))

(define (crazy2add-carry lhs rhs carry) ; crazy2add-carry: TODO
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: TODO
            (ltl (cdr lhs))) ; ltl: TODO
        (if (pair? rhs)
            (let* ((rhd (car rhs)) ; rhd: TODO
                  (rtl (cdr rhs)) ; rtl: TODO
                  (sum (zpnadd lhd rhd carry))) ; sum: TODO
              (cons (car sum) (crazy2add-carry ltl rtl (cdr sum))))
            (let ((sum (zpnadd lhd rhs carry))) ; sum: TODO
              (cons (car sum) (crazy2add-carry ltl 'z (cdr sum))))))
      (if (pair? rhs)
          (let* ((rhd (car rhs)) ; rhd: TODO
                (rtl (cdr rhs)) ; rtl: TODO
                (sum (zpnadd lhs rhd carry))) ; sum: TODO
            (cons (car sum) (crazy2add-carry 'z rtl (cdr sum))))
          (zpnadd lhs rhs carry))))

(define (crazy2add lhs rhs) ; crazy2add: TODO
  (crazy2add-carry lhs rhs 'z))

(define (zipper lhs rhs) ; zipper: TODO
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: TODO
            (ltl (cdr lhs))) ; ltl: TODO
        (if (pair? rhs)
            (let ((rhd (car rhs)) ; rhd: TODO
                  (rtl (cdr rhs))) ; rtl: TODO
              (cons lhd (cons rhd (zipper ltl rtl))))
            lhs))
      rhs))

(define (zipperN lists) ; zipperN: TODO
  (if (pair? lists)
      (let ((lhd (car lists)) ; lhd: TODO
            (ltl (cdr lists))) ; ltl: TODO
        (if (pair? lhd)
            (let ((llhd (car lhd)) ; llhd: TODO
                  (lltl (cdr lhd))) ; lltl: TODO
              (cons llhd (zipperN (append ltl (list lltl)))))
            (zipperN ltl)))
      '()))

(define (iter n f) ; iter: TODO
  (let ((m (abs n))) ; m: TODO
    (if (equal? m 0)
        (lambda (x) x)
        (lambda (x)
          (let ((fx (f x)) ; fx: TODO
                (remf (iter (- m 1) f))) ; remf: TODO
            (remf fx))))))
