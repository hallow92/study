#lang racket

; switch the require for testing record-function.rkt
(require "record-func.rkt")
; (require "record-function.rkt")

(define dic1 (empty-record))
(define dic2 (add-record "kwang" "Professor" dic1))
(define dic3 (add-record "sungkeun" "TA" dic2))
(define dic4 (add-record "jeehoon" "TA" dic3))

(print (find-record "kwang" dic4)) ; "Professor"
(printf "\n")
(print (find-record "sungkeun" dic4)) ; "TA"
(printf "\n")
(print (find-record "jeehoon" dic4)) ; "TA"
(printf "\n")

(define dic5 (add-record "kwang" "Awesome Professor" dic4))
(define dic6 (add-record "sungkeun" "Awesome TA" dic5))
(define dic7 (add-record "jeehoon" "Awesome TA" dic6))

(print (find-record "kwang" dic7)) ; "Awesome Professor"
(printf "\n")
(print (find-record "sungkeun" dic7)) ; "Awesome TA"
(printf "\n")
(print (find-record "jeehoon" dic7)) ; "Awesome TA"
(printf "\n")