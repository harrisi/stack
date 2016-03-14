#lang racket
;;(require data/collection)

;; Push elem onto stack (default value null).
(define (push elem [stack null])
  (cons elem stack))

;; Pop item off stack, returning the stack after pop.
(define (pop stack)
  (unless (empty? stack)
    (rest stack)))

;; Peek at top element of stack, returning it.
(define (peek stack)
  (unless (null? stack)
    (first stack)))

;; Run a program defined as a stack.
(define (run prog)
  ;; Internal stack.
  (define stack null)
  ;; for each element in program...
  (for ([elem prog])
    ;; if current element is a procedure..
    (if (procedure? elem)
        ;; local bindings for the top two elements..
        (let ([num1 (peek stack)]
              [num2 (peek (pop stack))])
          ;; set! the internal stack to be the current stack with top two
          ;; elements popped off and the result of applying current elem to them
          ;; pushed to stack.
          (set! stack (push (elem num1 num2) (pop (pop stack)))))
        ;; Otherwise, set! the internal stack to the result of pushing the
        ;; current element to the stack.
        (set! stack (push elem stack))))
  stack)

;; Old (start) of implementing run with cond. This is nice because there's not
;; arbitrary code execution, but it makes adding functionality more work.
;; I might continue to update this with "internal" functions as a "safe" option
;; to go alongside the above "wild west" implementation.
(define (run_old prog ns)
  (define stack null)
  (define func-table
    (make-immutable-hash '([~ . (lambda (n) (- n))]
                           [out . (lambda (o) (displayln o))])))
  (define arity-table
    (make-immutable-hash '([+ . 2]
                           [* . 2]
                           [- . 2] ;; binary subtraction
                           [/ . 2]
                           [~ . 1] ;; unary negation
                           [out . 1]))) ;; print top elem of stack
  (define (do-func f args)
    (cond
      ([hash-has-key? func-table f]
        (apply (eval (hash-ref func-table f) ns) args))
      ([primitive? (eval f ns)]
          (apply (eval f ns) args))
      (else
       (error
        (format "ERROR (do-func): function not primitve or language: ~a" f)))))
    
  (for ([elem prog])
    (cond
      ([or (hash-has-key? arity-table elem) (hash-has-key? func-table elem)]
       (let* ([arity (hash-ref arity-table elem)]
              [args (take stack arity)])
         (set! stack (push (do-func elem args)))
         (drop prog (add1 arity))))
      ([number? elem]
       (set! stack (push elem stack)))
      (else
       (error (format "ERROR: Cannot read term: ~a" elem)))))
  stack)

#;(define foo
  (for ([elem prog])
    (cond
      ;;  Token = +
      ([eq? elem '+]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (+ num1 num2) (pop (pop stack))))))
      
      ;; Token = *
      ([eq? elem '*]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (* num1 num2) (pop (pop stack))))))
      
      ;; Token = -
      ([eq? elem '-]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (- num1 num2) (pop (pop stack))))))

      ;; Token = /
      ([eq? elem '/]
       (let ([num1 (peek stack)]
             [num2 (peek (pop stack))])
         (set! stack (push (/ num1 num2) (pop (pop stack))))))

      ([number? elem]
       (set! stack (push elem stack)))
      ;; Token is literal
      (else
       (error (format "ERROR: Cannot read term: ~a." elem)))))
  stack)

;; REPL
(define (start ns)
  (define old false)
  (let/ec break
    (let loop ()
      (display "stacket> ")
      (define input (read))
      (cond
        ([eq? input 'q] ;; Quit REPL
         (break))
        ([eq? input 'o] ;; Toggle "old" vs "new" mode
         (set! old (not old))
         (loop)))
      (if (not old)
          (println (run (eval input ns)))
          (println (run_old input ns)))
      (loop)))
  (displayln "exiting"))

;; Entry point when run as `racket stack.rkt`
(module+ main
  (define-namespace-anchor anc)
  (define ns (namespace-anchor->namespace anc))
  (start ns))
