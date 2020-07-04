;; syntax utils
(define label? symbol?)
(define (goto? line) (tagged-list? line 'goto))
(define (branch? line) (tagged-list? line 'branch))
;; TOOD: static-goto? should return the label
(define (static-goto? line) (or (goto? line) (branch? line)))
(define (assignment? line) (tagged-list? line 'assign))
(define (test? line) (tagged-list? line 'test))

(define (label-assignment? line)
  (and (assignment? line)
       (= 3 (length line))
       (label-exp? (caddr line))))

(define (static-goto-to line label)
  (and (static-goto? line)
       (eq? label (goto-dest line))))


;;; data structure utils
(define (set-insert element set)
  (if (memq element set)
      set
      (cons element set)))

(define (set-union . sets)
  (cond ((= 1 (length sets)) (car sets))
        ((null? (car sets)) (set-union (cdr sets)))
        (else
         (set-union (cdar sets)
                    (set-insert (caar sets) (cadr sets))
                    .
                    (cddr sets)))))

;; dict: list of (key . val) pairs
;; (define (add-val-to-dict alist key val)
;;   (add-val-list-to-dict alist key (list val)))

;; (define (add-val-list-to-dict alist key val-list)
;;   (cond ((null? alist)
;;          (list (cons key val-list)))
;;         ((eq? (caar alist) key)
;;          (cons
;;           (insert-val-list (car alist) val-list)
;;           (cdr alist)))
;;         (else
;;          (cons
;;           (car alist)
;;           (add-val-list-to-dict (cdr alist) key val-list)))))

;; (define (insert-val-list keylist val-list)
;;   (let ((key (car keylist))
;;         (cur-vals (cdr keylist)))
;;     (cons key (append val-list cur-vals))))

;;; misc
(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define inc ++)
(define dec --)

(define (all vals)
  (cond ((null? vals) #t)
        ((not (car vals)) #f)
        (else (all (cdr vals)))))

(define (any vals)
  (cond ((null? vals) #f)
        ((car vals) #t)
        (else (any (cdr vals)))))
