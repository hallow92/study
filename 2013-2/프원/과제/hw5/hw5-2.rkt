#lang racket


;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)


; You can use the definitions and functions defined in hw5-1.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-1.rkt")


;;; interfaces
(define (equal f g) ; equal: form * form -> form
  (cond 
    ((and (is-array? f) (is-array? g)) (equal? f g))
    ((and (is-array? f) (is-tree? g)) (equal? f (tree-to-array g)))
    ((and (is-tree? f) (is-array? g)) (equal? (tree-to-array f) g))
    ((and (is-tree? f) (is-tree? g)) (equal? f g))
    )
  )

(define (size f) ; size: form -> int
  (if (is-array? f)
      (inexact->exact (/ (log (length f)) (log 2)))
      (size (tree-to-array f))
      ))

(define (beautiful f) ; beautiful: form -> bool
  (define (beautiful-sym f)
    #f)
  (define (beautiful-neighbor f)
    (if (is-tree? f)
        (beautiful-neighbor (tree-to-array f))
        (for/and ((i (length (remove 'array f))))
          (for/and ((j (length (remove 'array f))))
            (and (> (neighbor-by-pos (cons i j) (remove 'array f)) 1) (< (neighbor-by-pos (cons i j) (remove 'array f)) 6))
            )
          )
        ))
  (or (beautiful-sym f) (beautiful-neighbor f)))