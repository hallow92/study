 #lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (maze-check maze start end)
  (raise "TODO"))

(define (can-visit maze room)
  (let (can-enter-list (can-enter room maze))
    (if (null? can-enter-list)
        empty-set
        (if (is-member? (car can-enter-list) )(add-elemient (car can-enter-list) (can-visit maze (cdr can-enter-list))))
        )
    )
  )

(define (visiter room room-list)
  (if (null? room-list)
      empty-set
      (add-element (car room-list) (cdr room-list))
      )
  )