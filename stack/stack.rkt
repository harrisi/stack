#lang racket

<<<<<<< HEAD
=======
;; (require racket/cmdline)


>>>>>>> 4b865a238c41cc6ef875a04049413f49033b5559
(define (push elem [stack null])
  (cons elem stack))

(define (pop stack)
<<<<<<< HEAD
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
=======
  (if (null? stack)
      '()
      (rest stack)))

(define (peek stack)
  (if (null? stack)
      '()
      (first stack)))

(define (run prog)
  (eval (foldl cons null prog)))
>>>>>>> 4b865a238c41cc6ef875a04049413f49033b5559
