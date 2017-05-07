;;; data structure utils
(define (set-add-element element set)
  (if (memq element set)
      set
      (cons element set)))

(define (set-union a b)
  (if (null? a)
      b
      (set-union (cdr a) (set-add-element (car a) b))))

;; key-list: list of (key . vals) pairs
(define (add-val-to-key-list alist key val)
  (add-val-list-to-key-list alist key (list val)))

(define (add-val-list-to-key-list alist key val-list)
  (cond ((null? alist)
         (list (cons key val-list)))
        ((eq? (caar alist) key)
         (cons
          (insert-val-list (car alist) val-list)
          (cdr alist)))
        (else
         (cons
          (car alist)
          (add-val-list-to-key-list (cdr alist) key val-list)))))

(define (insert-val-list keylist val-list)
  (let ((key (car keylist))
        (cur-vals (cdr keylist)))
    (cons key (append val-list cur-vals))))

;;; misc
(define (++ n) (+ n 1))
(define (-- n) (- n 1))
