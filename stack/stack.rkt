#lang racket

(define (push elem [stack null])
  (cons elem stack))

(define (pop stack)
  (unless (empty? stack)
    (rest stack)))

(define (peek stack)
  (unless (null? stack)
    (first stack)))

(define (run prog)
  (define stack null)
  (for ([iprog prog])
    (if (procedure? iprog)
        (let ([num1 (peek stack)]
              [num2 (peek (pop stack))])
          (set! stack (push (iprog num1 num2) (pop (pop stack)))))
        (set! stack (push iprog stack))))
  (println stack))

(define (run_old prog)
  (define stack null)
  (for ([iprog prog])
    (cond
      ;;  Token = +
      ([eq? iprog '+]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (+ num1 num2) (pop (pop stack))))))
      
      ;; Token = *
      ([eq? iprog '*]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (* num1 num2) (pop (pop stack))))))
      
      ;; Token = -
      ([eq? iprog '-]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (- num1 num2) (pop (pop stack))))))
      
      ;; Token is literal
      (else
       (set! stack (push iprog stack)))))
  (println stack))