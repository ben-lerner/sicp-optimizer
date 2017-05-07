(define label? symbol?)
(define (goto? line) (tagged-list? line 'goto))
(define (branch? line) (tagged-list? line 'branch))
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
