;; syntax utils
(define label? symbol?)
(define (goto? line) (tagged-list? line 'goto))
(define (branch? line) (tagged-list? line 'branch))
;; TOOD: static-goto? should return the label
(define (static-goto? line) (or (goto? line) (branch? line)))
(define (assignment? line) (tagged-list? line 'assign))
(define (test? line) (tagged-list? line 'test))

;; path-making util
(define (label-assignment? line)
  (and (assignment? line)
       (= 3 (length line))
       (label-exp? (caddr line))))

;;; data structure utils
(define (set-insert set element)
  (if set
      (if (memq element set)
          set
          (cons element set))
      (list element)))

(define (set-union . sets)
  (cond ((= 1 (length sets)) (car sets))
        ((null? (car sets)) (apply set-union (cdr sets)))
        (else
         (apply set-union
                `(,(cdar sets)
                  ,(set-insert (cadr sets) (caar sets))
                  ,@(cddr sets))))))

;; dict: list of (key . val) pairs
(define (insert dict key val)
  (cons (cons key val) dict))

(define (get dict key)
  (let ((pair (assoc key dict)))
    (if pair
        (cdr pair)
        #f)))

;; dict-of-sets: list of (key . set) pairs
(define (dict-of-sets-insert dict-of-sets key val)
  ;; note: this grows with each insert
  (insert dict-of-sets
          key
          (set-insert (get dict-of-sets key) val)))

;;; misc
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (all vals)
  (cond ((null? vals) #t)
        ((not (car vals)) #f)
        (else (all (cdr vals)))))

(define (any vals)
  (cond ((null? vals) #f)
        ((car vals) #t)
        (else (any (cdr vals)))))
