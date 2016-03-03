#lang racket

;; (require racket/cmdline)


(define (push elem [stack null])
  (cons elem stack))

(define (pop stack)
  (if (null? stack)
      '()
      (rest stack)))

(define (peek stack)
  (if (null? stack)
      '()
      (first stack)))

(define (run prog)
  (eval (foldl cons null prog)))