 #lang racket

; DO NOT ADD OTHER COMMENTS.
; MODIFY ONLY "TODO"s in the following comments.

(define (gcd n m) ; gcd: int * int -> int
  (if (equal? n 0) m
      (gcd (remainder m n) n)))

(define (zpn x) ; zpn: symbol -> int
  (cond ((equal? x 'z) 0)
        ((equal? x 'p) 1)
        ((equal? x 'n) -1)))

(define (crazy2val c) ; crazy2val: symbol list -> int
  (cond ((pair? c)
         (let ((hd (car c)) ; hd: symbol
               (tl (cdr c))) ; tl: symbol list
           (+ (* 2 (crazy2val tl)) (zpn hd))))
        (else (zpn c))))

(define (zpnadd a b c) ; zpnadd: symbol * symbol * symbol -> symbol X symbol
  (let ((sum (+ (zpn a) (zpn b) (zpn c)))) ; sum: int
    (cond ((equal? sum 3) (cons 'p 'p))
          ((equal? sum 2) (cons 'z 'p))
          ((equal? sum 1) (cons 'p 'z))
          ((equal? sum 0) (cons 'z 'z))
          ((equal? sum -1) (cons 'n 'z))
          ((equal? sum -2) (cons 'z 'n))
          ((equal? sum -3) (cons 'n 'n)))))

(define (crazy2add-carry lhs rhs carry) ; crazy2add-carry: symbol list * symbol list * symbol -> symbol list
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: symbol
            (ltl (cdr lhs))) ; ltl: symbol list
        (if (pair? rhs)
            (let* ((rhd (car rhs)) ; rhd: symbol
                  (rtl (cdr rhs)) ; rtl: symbol list
                  (sum (zpnadd lhd rhd carry))) ; sum: symbol X symbol
              (cons (car sum) (crazy2add-carry ltl rtl (cdr sum))))
            (let ((sum (zpnadd lhd rhs carry))) ; sum: symbol X symbol
              (cons (car sum) (crazy2add-carry ltl 'z (cdr sum))))))
      (if (pair? rhs)
          (let* ((rhd (car rhs)) ; rhd: TODO
                (rtl (cdr rhs)) ; rtl: TODO
                (sum (zpnadd lhs rhd carry))) ; sum: TODO
            (cons (car sum) (crazy2add-carry 'z rtl (cdr sum))))
          (zpnadd lhs rhs carry))))

(define (crazy2add lhs rhs) ; crazy2add: TODO
  (crazy2add-carry lhs rhs 'z))

(define (zipper lhs rhs) ; zipper: int list * int list -> int list
  (if (pair? lhs)
      (let ((lhd (car lhs)) ; lhd: int
            (ltl (cdr lhs))) ; ltl: int list
        (if (pair? rhs)
            (let ((rhd (car rhs)) ; rhd: int
                  (rtl (cdr rhs))) ; rtl: int list
              (cons lhd (cons rhd (zipper ltl rtl))))
            lhs))
      rhs))

(define (zipperN lists) ; zipperN: int list의 list -> int list
  (if (pair? lists)
      (let ((lhd (car lists)) ; lhd: int list
            (ltl (cdr lists))) ; ltl: int list의 list
        (if (pair? lhd)
            (let ((llhd (car lhd)) ; llhd: int list
                  (lltl (cdr lhd))) ; lltl: int list의 list
              (cons llhd (zipperN (append ltl (list lltl)))))
            (zipperN ltl)))
      '()))

(define (iter n f) ; iter: int * function -> function
  (let ((m (abs n))) ; m: int
    (if (equal? m 0)
        (lambda (x) x)
        (lambda (x)
          (let ((fx (f x)) ; fx: 
                (remf (iter (- m 1) f))) ; remf: TODO
            (remf fx))))))
