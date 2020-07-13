(cd "compiler")  ;; Scheme's library utilties are not great
(load "5-compiler.scm")  ;; SICP code for compiler, register machine, and syntax
(cd "..")

(load "utils.scm")
(load "assembly-test.scm")  ;; for testing

;; data structures:
;;; vals:
;;; list of assoc-lists of the form (reg: (possible-vals))
;;;      undefined is reg: *unassigned*
;;;      nth list corresponds to possible vals at nth LOC.
;;;      possible vals are potential assigned constants.
;;; types:
;;; list of assoc-lists, as above, but for types. used for op.

;;; label-paths:
;;; assoc-list
;;;    label -> lines that go to the label
;;;    *register* -> lines that go a register location
;;;    *register-labels* -> labels that go into registers
;;; needed-reg:
;;; list of lists, nth list is any register that is read past that list, and
;;; therefore can't be optimized out. We assume 'val' is needed on the last line.
;;;
;;; notes:
;;; some registers should start with "start", others with "unassigned":
;;;; maybe? look at how compiled code is linked and re-eval?
;;;; start: env, continue, val
;;;; proc: unassigned, argl, arg1, arg2?


;; If a value is written to one of these registers, we want to leave it there
;; (because it may be read by the user). Otherwise, elide unused reads.
(define important-registers '(val))

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
  (make code '() '() '() 0 #f))

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
  (define (fold-line exp)
    (define (-fold-line exp)
      (cond ((not (pair? exp)) exp)
            ((null? exp) exp)
            ((not (operation-exp? exp))
             (cons (car exp) (-fold-line (cdr exp))))
            (else ;; found op - apply primitive op to constants
             (let ((op (assoc (operation-exp-op exp) ops))
                   (args (operation-exp-operands exp)))
               (if (and ops (all (map constant-exp? args)))
                   `((const ,(apply (cadr op) (map cadr args))))
                   exp)))))
    (let ((exp (-fold-line exp)))
      ;; add (op true?) to folded test for a valid exp. This will be optimized
      ;; away later, but we need it for the instruction to be valid.
      (if (and (test? exp) (= 2 (length exp)))
          `(test (op true?) ,@(cdr exp))
          exp)))
  (map fold-line code))

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


;; Returns a list, paths = '((1) (2) (3 4) ... (EOF)), where (nth paths n) is
;; all lines that the nth line can go to. Note that code lines are 0-indexed.
(define (code-paths code)
  (define (label-index code)  ;; label -> that label's line number
    (define (-label-index code line-number index)
      (if (null? code)
          index
          (-label-index
           (cdr code)
           (inc line-number)
           (if (label? (car code))
               (insert index (car code) line-number)
               index))))

    (-label-index code 0 '()))

  (let ((label-dict (label-index code))
        (lines-to-labels (invert (make-label-paths code))))
    (define (transitions line line-number)
      (set-union
       (if (goto? line) '() (list (inc line-number)))
       (filter identity
               (map
                (lambda (label) (get label-dict label))
                (or (get lines-to-labels line-number) '())))))

    ;; todo - do we need to write down when START goes to a label at the
    ;; beginning of the code?

    (define (-code-paths code line-number)
      (if (null? code)
          '()
          (cons (transitions (car code) line-number)
                (-code-paths (cdr code) (inc line-number)))))

    (-code-paths code 0)))


;; todo - factor out non-label one

;; starting from start-line, walks through code and checks if (read line)
;; is ever true before (write line) (ignoring start-line).
;; e.g., test if a register is read before it's written again.
;; read-at-end is true if reaching the end of code is considered a read
;; (e.g. for val).
(define (read-before-write? code start-line read write read-at-end)
  ;; find all code transitions
  ;; This should reuse extract-labels, but we want to index the labels without
  ;; extracting them.



  (let ((paths (code-paths code 1)))
    paths
    ))
