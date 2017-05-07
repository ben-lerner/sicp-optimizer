;;; lexical scanning: 5-39 thru 5-41
(define (make-new-c-env) '())
(define (extend-c-env c-env args) (cons args c-env))
(define (enclosing-c-env c-env) (cdr c-env))
(define (first-var c-frame) (car c-frame))
(define (rest-vars c-frame) (cdr c-frame))
(define (first-frame c-env) (car c-env))
(define (next-frame c-env) (first-frame (enclosing-c-env c-env)))

(define (nth-tail list n)
  (if (= n 0)
      list
      (nth-tail (cdr list) (- n 1))))

(define (nth list n) (car (nth-tail list n)))

(define (nth-head list n)
  (if (= n 0)
      '()
      (cons (car list) (nth-head (cdr list) (- n 1)))))

(define (lexical-address-lookup address env)
  (define (lookup frame-index element-index env)
    (if (> frame-index 0)
        (lookup (- frame-index 1) element-index (enclosing-environment env))
        (let ((val (nth (frame-values (first-frame env)) element-index)))
          (if (eq? val '*unassigned*)
              (error "variable is unnasigned")
              val))))
  (lookup (car address) (cdr address) env))

(define (lexical-address-set! value address env)
  (define (lexical-set! frame-index element-index env)
    (if (> frame-index 0)
        (lexical-set! (- frame-index 1) element-index (enclosing-environment env))
        (set-car! (nth-tail (frame-values  (first-frame env)) element-index) value)))
  (lexical-set! (car address) (cdr address) env))

(define (find-variable var c-env)
  (define (iter frame-index var-index c-frame c-env)
    (cond ((null? c-env) 'not-found)
          ((null? c-frame)
           (if (null? (enclosing-c-env c-env))
               'not-found
               (iter (+ 1 frame-index) 0 (next-frame c-env) (enclosing-c-env c-env))))
          ((eq? var (first-var c-frame))
           (cons frame-index var-index))
          (else
           (iter frame-index (+ 1 var-index) (rest-vars c-frame) c-env))))
  (if (null? c-env)
      'not-found
      (iter 0 0 (first-frame c-env) c-env)))

(define lexical-exp
  '(((lambda (x y)
        (lambda (a b c d e)
          ((lambda (y z) (* x y z))
           (* a b x)
           (+ c d x))))
      3 4) 1 2 3 4 5))

(define simple-lexical-exp
  '((lambda (x y)
      (+ x y))
    1 2))

(define lexical-exp2
  '(((lambda (x)
        (lambda (y) (+ x y)))
      1) 2))

(define lexical-exp3
  '(((lambda (x)
        (lambda (x) (+ x x)))
      1) 2))

(define lexical-exp4
  '(((lambda (x y)
        (lambda (a b c d e)
          ((lambda (y z) (+ x y z))
           (+ a b x)
           (+ c d x))))
     1 10) 100 1000 10000 100000 1000000))
