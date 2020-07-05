;; syntax utils
(define label? symbol?)
(define (goto? line) (tagged-list? line 'goto))
(define (branch? line) (tagged-list? line 'branch))
;; TOOD: static-goto? should return the label
(define (static-goto? line) (or (goto? line) (branch? line)))
(define (assignment? line) (tagged-list? line 'assign))

(define (test? line) (tagged-list? line 'test))
(define (identity x) x)

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

(define (set . elements)
  (if (null? elements)
      '()
      (set-insert (apply set (cdr elements)) (car elements))))

;; dict: list of (key . val) pairs
(define (insert dict key val)
  (cons (cons key val)
        (filter (lambda (pair) (not (equal? (car pair) key))) dict)))

(define (get dict key)
  (cond ((null? dict) #f)
        ((equal? (caar dict) key) (cdar dict))
        (else (get (cdr dict) key))))

;; dict-of-sets: list of (key . set) pairs
(define (dict-of-sets-insert dict-of-sets key val)
  (insert dict-of-sets
          key
          (set-insert (get dict-of-sets key) val)))

(define (dict-to-sets-to-pairs dict-to-sets)
  (apply append (map
                 (lambda (pair)
                   (let ((key (car pair))
                         (set (cdr pair)))
                     (map (lambda (v) (cons key v)) set)))
                 dict-to-sets)))

(define (pairs-to-dict-to-sets pairs)
  (if (null? pairs)
      pairs
      (dict-of-sets-insert
       (pairs-to-dict-to-sets (cdr pairs))
       (caar pairs)
       (cdar pairs))))

;; alternate definition of pairs-to-dict-to-sets.
;; this one is conceptually simpler, but harder to follow with scheme's reduce
(define (p2 pairs)
  (reduce
   (lambda (pair d)
     (dict-of-sets-insert d (car pair) (cdr pair)))
   '()
   (cons '() pairs)))

(define (invert dict-to-sets)
  (pairs-to-dict-to-sets
   (map (lambda (pair) (cons (cdr pair) (car pair)))
        (dict-to-sets-to-pairs dict-to-sets))))
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
