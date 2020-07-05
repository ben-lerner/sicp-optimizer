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
;;;
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
    (let ((labels (apply set-union
                          (cons
                           (map car label-to-lines)
                           (map cdr reg-to-labels))))
          (labels-to-reg (invert reg-to-labels)))
      ;; label -> union (label-to-lines, relevant-reges-to-lines)
      (map
       (lambda (label)
         (cons label
               (let ((registers (get labels-to-reg label)))
                 (if (not registers)
                     (get label-to-lines label)  ;; must be non-empty
                     (apply set-union
                            (cons (or (get label-to-lines label) '())
                                  (map (lambda (r) (get reg-to-lines r)) registers)))))))
       labels)))
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
    (filter identity (map update-labels lines)))

  (change-labels lines (redundant-labels lines)))

;; construct data structures
(define (get-registers code)
  (apply set-union (map get-registers-from-line code)))

;; delete labels with no lines going to them
(define (label-cleanup code)
  (let ((paths (make-label-paths code)))
    (filter
     (lambda (line)
       (or (not (label? line))
           (get paths line)))
     code)))

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
(define (get-registers-from-line line)
  (define (reg-pairs line)
    (apply set
           (filter
            identity
            (map
             (lambda (exp)
               (and (register-exp? exp)
                    (register-exp-reg exp)))
             line))))
  (cond ((label? line) '())
        ((assignment? line)
         (set-insert (reg-pairs line) (assign-reg-name line)))
        (else (reg-pairs line))))

;; constant folding

;;; normally, we would get primitive operations from the machine.
;;; for convenience, we're inlining the list of ops.
(define ops
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '= =)
        (list 'false? false?)
        (list 'true? true?)))

(define (fold-constants code)
  (define (fold-line line)
    (if (and (assignment-of-op? line)
             (assoc (assignment-op line) ops)
             (all (map constant-exp? (assignment-args line))))
        `(assign
          ,(assign-reg-name line)
          (const ,(apply (cadr (assoc (assignment-op line) ops))
                         (map cadr (assignment-args line)))))
        line))
  (map fold-line code))
