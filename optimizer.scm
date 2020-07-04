(cd "compiler")  ;; Scheme's library utilties are not great
(load "5-compiler.scm")  ;; SICP code for compiler, register machine, and syntax
(cd "..")

(load "utils.scm")

;; data structures:
;;; vals:
;;; list of assoc-lists of the form (reg: (possible-vals))
;;;      undefined is reg: *unassigned*
;;;      nth list corresponds to possible vals at nth LOC.
;;;      possible vals are potential assigned constants.
;;; types:
;;; list of assoc-lists, as above, but for types. used for op.
;;; TODO:
;;; label-paths:
;;; assoc-list
;;;    label -> lines that go to the label
;;;    *register* -> lines that go a register location
;;;    *register-labels* -> labels that go into registers
;;; needed-reg:
;;; list of lists, nth list is any register that is read past that list, and
;;; therefore can't be optimized out. We assume 'val' is needed on the last line.
;;; TODO: make list of needed registers configurable.
;;;
;;; notes:
;;; some registers should start with "start", others with "unassigned":
;;;; maybe? look at how compiled code is linked and re-eval?
;;;; start: env, continue, val
;;;; proc: unassigned, argl, arg1, arg2?

;;;; label cleanup
(define (make-label-paths code)
  ;; return {label: {i, j, ...}},
  ;;   where {i, j, ...} is every line that may go to label

  (define (make
            code
            label-to-lines  ;; goto or branch to label
            reg-to-lines    ;; goto or branch to register
            reg-to-labels   ;; labels assigned to register
            line-number
            last-line-was-goto)
    (if (null? code)
        (combine-dicts label-to-lines reg-to-lines reg-to-labels)
        (let ((line (car code)))
          (make
            (cdr code)
            ;; label-to-lines
            (cond ((and (static-goto? line) (label-exp? (goto-dest line)))
                   (dict-of-sets-insert label-to-lines
                                        (label-exp-label (goto-dest line))
                                        line-number))
                  ((and (label? line) (not last-line-was-goto))
                   (dict-of-sets-insert label-to-lines
                                        line
                                        (dec line-number)))
                  (else label-to-lines))

            ;; reg-to-lines
            (if (and (static-goto? line) (register-exp? (goto-dest line)))
                (dict-of-sets-insert reg-to-lines
                                     (register-exp-reg (goto-dest line))
                                     line-number)
                reg-to-lines)

            ;; reg-to-labels
            (if (label-assignment? line)
                (dict-of-sets-insert reg-to-labels
                                     (assign-reg-name line)
                                     (label-exp-label
                                      (car (assign-value-exp line))))
                reg-to-labels)

            (inc line-number)
            (goto? line)))))

  (define (combine-dicts label-to-lines reg-to-lines reg-to-labels)
    (let ((labels (map car label-to-lines))))
    label-to-lines


    )

  (make code '() '() '() 1 #f))

(define (fuse-consecutive-labels lines)
  (define (redundant-labels lines)
    (define (-redundant-labels lines new-line-dict last-label)
      (cond ((null? lines) new-line-dict)
            ((label? (car lines))
             (if last-label
                 ;; two lables in a row
                 (-redundant-labels
                  (cdr lines)
                  (insert new-line-dict (car lines) last-label)
                  last-label)
                 ;; not two lables in a row
                 (-redundant-labels
                  (cdr lines)
                  new-line-dict
                  (car lines))))
            (else (-redundant-labels (cdr lines) new-line-dict #f))))
    (-redundant-labels lines '() #f))

  (define (change-labels lines new-line-dict)
    (define (update-label label)  ;; new-line-dict[label] or label
      (let ((el (assoc label new-line-dict)))
        (if el (cdr el) label)))
    (define (update-labels line)
      (if (label? line)
          (if (get new-line-dict line)    ;; skip duplicates
              #f  ;; #f filtered out later
              line)
          (map
           (lambda (exp)
             (if
              (label-exp? exp)
              `(label ,(update-label (label-exp-label exp)))
              exp))
           line)))
    (filter (lambda (x) x) (map update-labels lines)))

  (change-labels lines (redundant-labels lines)))

;; construct data structures
(define (get-registers code)
  (set-union (map get-registers-from-line code)))

;; clean labels: delete labels with no lines going to them

(define (paths-for-label line paths)
  (if (not (label? line))
      #f
      (let ((p (assoc line paths)))
        (if p
            p
            '()))))

(define (label-cleanup code)
  (let ((paths (make-label-paths code)))
    (define (-label-cleanup reversed-processed-code code i after-goto)
      (if (null? code) (reverse reversed-processed-code)
          (let* ((line (car code))
                 (p (paths-for-label line paths)))
            ;; returns #f if line isn't a label
            (-label-cleanup
             (if (or
                  (null? p)
                  (and (equal? (list i) p)
                       (not (equal? after-goto line))))
                 reversed-processed-code
                 (cons line reversed-processed-code))
             (cdr code)
             (inc i)
             (and
              (static-goto? line)
              (static-goto-label line))))))
    (-label-cleanup '() code 0 #f)))


;; drop code between goto and a label
(define (unreachable-code-cleanup code)
  (define (drop-helper code looking-for-label)
    (if (null? code) '()
        (let ((first (car code))
              (rest (cdr code)))
          (cond ((label? first) (cons first (drop-helper rest false)))
                (looking-for-label (drop-helper rest true))
                ((goto? first) (cons first (drop-helper rest true)))
                (else (cons first (drop-helper rest false))); we know !looking-for-label
                ))))
  (drop-helper code false))

(define (branch-test-cleanup code)
  (define (helper reversed-code branch-seen)
    ;; branch-seen tracks whether we've seen a branch since the last test. If
    ;; not, we can drop the test.
    (if (null? reversed-code)
        '()
        (let ((first (car reversed-code))
              (rest (cdr reversed-code)))
          (cond ((branch? first) (cons first (helper rest true)))
                ((test? first)
                 (if branch-seen
                     (cons first (helper rest false))
                     (helper rest false)))
                (else (cons first (helper rest branch-seen)))))))
  (reverse (helper (reverse code) false)))

;; drop gotos and branches that skip no code
;; ignore (goto reg). if `reg` is constant it'll be replaced with a label
;; in a different pass, and this will work in a later iteration.
(define (goto-cleanup code)
  (if (<= (length code) 1)
      code
      (let ((first (car code))
            (second (cadr code)))
        (if (noop-static-goto first second)
            (cons second (goto-cleanup (cddr code)))
            (cons first (goto-cleanup (cdr code)))))))

(define (noop-static-goto first second)
  (and (static-goto? first)
       (label-exp? (goto-dest first))
       (label? second)
       (eq? second (label-exp-label (goto-dest first)))))


;; code manipulation utils
(define  (get-registers-from-line line)
  (define (line-helper parts)
    (cond ((null? parts) '())
          ((not (list? (car parts))) ;; assign
           (set-insert (car parts) (line-helper (cdr parts))))
          ((eq? (caar parts) 'reg)
           (set-insert (cadar parts) (line-helper (cdr parts))))
          (else
           (line-helper (cdr parts)))))

  (if (label? line)
      '()
      (line-helper (cdr line)) ;; skip op
      ))
