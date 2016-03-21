#!/usr/local/bin/racket
#lang racket

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

;; This was originally the initial implementation (roughly), but that
;; implementation was moved below to `foo`. The current approach is somewhat
;; nicer in the sense that (1) functions aren't restricted to binary operations,
;; and (2) adding functions (or changing arity) only requires updating a hash,
;; rather than implementing a whole new `cond` branch.
(define (run_old prog ns)
  (define stack null)
  (define func-table
    (make-immutable-hash '([~ . (lambda (n) (- n))]
                           [out . displayln]
                           [++ . string-append]
                           [in . read]
                           [prompt . (lambda (o) (displayln o) (read))])))
  (define arity-table
    (make-immutable-hash '([+ . 2]
                           [* . 2]
                           [- . 2] ;; binary subtraction
                           [/ . 2]
                           [~ . 1] ;; unary negation
                           [out . 1] ;; print top elem of stack
                           [++ . 2] ;; string concatenation
                           [in . 0] ;; read input
                           [prompt . 1]
                           [number->string . 1]))) ;; display str and get input
  (define (do-func f args)
    (cond
      ;; if elem is in `func-table`, it is a "language" function, rather than a
      ;; "primitive" function, which just uses the racket function itself
      ;; (e.g. +, 0, *)
      ([hash-has-key? func-table f]
       ;; language functions' definitions are defined as the function associated
       ;; with the key in `func-table`
       (apply (eval (hash-ref func-table f) ns) args))
      ;; if elem is primitive (a racket function), we don't need the function
      ;; defined in `func-table`. Note that this does not mean that arbitrary
      ;; functions are allowed (e.g. `++` is defined as `string-append`, but one
      ;; cannot call `string-append` in place of `++`).
      ([primitive? (eval f ns)]
       ;; simply apply the function to args (based on arity)
       (apply (eval f ns) args))
      (else
       (error
        (format "ERROR (do-func): function not primitve or language: ~a" f)))))
  
  (for ([elem prog])
    (cond
      ;; if elem is in arity-table, it is a function
      ([hash-has-key? arity-table elem]
       (let* ([arity (hash-ref arity-table elem)] ;; lookup arity in arity-table
              [args (take stack arity)]) ;; take (arity) args from stack
         (set! stack (drop stack arity)) ;; drop the args from the stack
         (set! stack ;; set stack to result of func pushed onto the stack
               (push (do-func elem args) stack))
         (when (void? (peek stack))
           (set! stack (pop stack)))))
      ([or (number? elem) (string? elem)] ;; is literal (num, string)
       (set! stack (push elem stack))) ;; push literal on stack
      (else ;; otherwise, panic.
       (error (format "ERROR: Cannot read term: ~a" elem)))))
  stack) ;; stack is result of program

;; Old (start) of implementing run with cond. This is nice because there's not
;; arbitrary code execution, but it makes adding functionality more work.
;; I might continue to update this with "internal" functions as a "safe" option
;; to go alongside the above "wild west" implementation.
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
  (if (file-exists? "ignore.stack")
      (run_old (file->value "ignore.stack") ns)
      (start ns)))