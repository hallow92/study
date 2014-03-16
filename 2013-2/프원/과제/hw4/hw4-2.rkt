#lang racket


; We auto-grade only react function; other functions are not
; auto-graded. However, S, K, I, v, and a are required for
; grading. See hw4-2-grade.rkt for more information.
; If this statement is omitted, your submission will be graded 0.
(provide react S K I v a)


; Implement react. The contents provided below can be modified,
; unless the modification does not change the type and satisfies the spec.
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".
(define (react solution) ; react: solution -> string
  (pprint (react_s solution))
  )

(define (react_s solution) ; react_s: solution -> solution
  (if (isa? solution)
      (cond 
        ((isI? (al solution)) (react_s (ar solution)))
        ((isv? (al solution)) (react_s (a (al solution) (react_s (ar solution)))))
        ((isa? (al solution))
         (cond
           ((isK? (al (al solution))) (react_s (ar solution)))
           ((isv? (al (al solution))) (a (a (al (al solution)) (react_s (ar (al solution)))) (react_s (ar solution))))
           ((isa? (al (al solution)))
            (cond
              ((isS? (al (al (al solution)))) (react_s (a (a (ar (al (al solution))) (ar solution)) (a (ar (al solution)) (ar solution)))))
              ((isv? (al (al (al solution)))) (a (a (a (al (al (al solution))) (react_s (ar (al (al solution))))) (react_s (ar (al solution)))) (react_s (ar solution))))
              (else (react_s (a (react_s (al solution)) (react_s (ar solution)))))
              )
            )
           )
         )
        )
      solution
      )
  )

(define S ; S: solution
  'S)
(define K ; K: solution
  'K)
(define I ; I: solution
  'I)
(define (v str) ; v: string -> solution
  (string->symbol str))
(define (a lhs rhs) ; v: solution * solution -> solution
  (cons lhs rhs))


; You may need the following tree interface.

(define (isS? solution) ; isS?: solution -> bool
  (equal? S solution))
(define (isK? solution) ; isK?: solution -> bool
  (equal? K solution))
(define (isI? solution) ; isI?: solution -> bool
  (equal? I solution))
(define (isv? solution) ; isv?: solution -> bool
  (and (symbol? solution) (not (isS? solution)) (not (isK? solution)) (not (isI? solution))))
(define (isa? solution) ; isa?: solution -> bool
  (pair? solution))
(define (var solution) ; var: solution -> string
  (symbol->string solution))
(define (al solution) ; al: solution -> solution
  (car solution))
(define (ar solution) ; ar: solution -> solution
  (cdr solution))
(define (pprint solution) ; pprint: solution -> string
  (if (isa? solution)
      (string-append "(" (pprint (al solution)) " " (pprint (ar solution)) ")")
      (cond
        ((isS? solution) "S")
        ((isK? solution) "K")
        ((isI? solution) "I")
        ((isv? solution) (var solution))
        )
      ))


; Write down types for each declaration and functions.
; -1 points for each wrong type annotation (-5 points top).
