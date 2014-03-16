#lang racket

(require "hw3-3-library.rkt")

; Play around the interface of hw3-3-library.rkt.
; After executing this file, see hw3-3.ps.
(define maze1 (init-maze 4 3))
(define maze2 (open-s 1 1 maze1))
(maze-pp maze2)

(define (mazeGen n m)
  (let ((maze (init-maze n m)))
    (raise "TODO")
    )
  )

(define (visitor n m visited_list my_stack maze x_max y_max)
  ()
  )
