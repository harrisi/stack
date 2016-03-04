# Stack implementation in Racket

There is no point to this repo. Just messing around with Racket.

## Usage

```
(run (list 4 5 + 6 * 10 -))
;; '(-44)

(run_old '(4 5 + 6 * 10 -))
;; '(-44)

;; note that run allows arbitrary functions:
(run (list 4 5 (lambda (a b) (+ a 2 (* b 5)))))
;; '(27)

(run_old '(4 5 (lambda (a b) (+ a 2 (* b 5)))))
;; '((lambda (a b) (+ a 2 (* b 5))) 5 4)
```
