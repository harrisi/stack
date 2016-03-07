#lang br
(define (read-syntax src-path src-port)
  (define src-strs (remove-blank-lines (port->lines src-port)))
  (define (make-datum str) (format-datum '(dispatch ~a) str))
  (define src-exprs (map make-datum src-strs))
  (inject-syntax ([#'(src-expr ...) src-exprs])
                 #'(module stacker-mod "stacker-lang.rkt"
                     src-expr ...)))
(provide read-syntax)

(define #'(stacker-module-begin reader-line ...)
  #'(#%module-begin
     reader-line ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)
(define (push val) (set! stack (cons val stack)))

(define (dispatch arg-1 [arg-2 #f])
  (cond
    [(number? arg-2) (push arg-2)]
    [else
     (define op arg-1)
     (define op-result (op (first stack) (second stack)))
     (set! stack (cons op-result (drop stack 2)))]))

(provide #%top-interaction)